/* Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains wrappers for the system allocation routines.  Most
   places in the OpenMP API do not make any provision for failure, so in
   general we cannot allow memory allocation to fail.  */

#define _GNU_SOURCE
#include "libgomp.h"
#if defined(PLUGIN_SUPPORT) && defined(LIBGOMP_USE_PTHREADS)
#define LIBGOMP_USE_MEMKIND
#endif

/* Implement malloc routines that can handle pinned memory on Linux.
   
   Given that pinned memory is typically used to help host <-> device memory
   transfers, we attempt to register such using a device (really: libgomp
   plugin), but fall back to mlock if no suitable device is available.

   It's possible to use mlock on any heap memory, but using munlock is
   problematic if there are multiple pinned allocations on the same page.
   Tracking all that manually would be possible, but adds overhead. This may
   be worth it if there are a lot of small allocations getting pinned, but
   this seems less likely in a HPC application.

   Instead we optimize for large pinned allocations, and use mmap to ensure
   that two pinned allocations don't share the same page.  This also means
   that large allocations don't pin extra pages by being poorly aligned.  */

#define _GNU_SOURCE
#include <sys/mman.h>
#include <string.h>
#include <assert.h>
#include "libgomp.h"

static bool always_pinned_mode = false;

/* This function is called by the compiler when -foffload-memory=pinned
   is used.  */

void
GOMP_enable_pinned_mode ()
{
  if (mlockall (MCL_CURRENT | MCL_FUTURE) != 0)
    gomp_error ("failed to pin all memory (ulimit too low?)");
  else
    always_pinned_mode = true;
}

static int using_device_for_register_page_locked
  = /* uninitialized */ -1;

static void *
linux_memspace_alloc (omp_memspace_handle_t memspace, size_t size, int pin)
{
  gomp_debug (0, "%s: memspace=%llu, size=%llu, pin=%d\n",
	      __FUNCTION__, (unsigned long long) memspace, (unsigned long long) size, pin);

  /* Explicit pinning may not be required.  */
  pin = pin && !always_pinned_mode;

  if (memspace == ompx_unified_shared_mem_space)
    {
      return gomp_usm_alloc (size);
    }
  else if (pin)
    {
      /* 'mmap' zero-initializes, which 'linux_memspace_calloc' relies on.  */
      void *addr = mmap (NULL, size, PROT_READ | PROT_WRITE,
			 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
      if (addr == MAP_FAILED)
	return NULL;

      int using_device
	= __atomic_load_n (&using_device_for_register_page_locked,
			   MEMMODEL_RELAXED);
      gomp_debug (0, "  using_device=%d\n",
		  using_device);
      if (using_device != 0)
	{
	  using_device = gomp_register_page_locked (addr, size);
	  int using_device_old
	    = __atomic_exchange_n (&using_device_for_register_page_locked,
				   using_device, MEMMODEL_RELAXED);
	  gomp_debug (0, "  using_device=%d, using_device_old=%d\n",
		      using_device, using_device_old);
	  assert (using_device_old == -1
		  /* We shouldn't have concurrently changed our mind.  */
		  || using_device_old == using_device);
	}
      if (using_device == 0)
	{
	  gomp_debug (0, "  mlock\n");
	  if (mlock (addr, size))
	    {
	      gomp_debug (0, "libgomp: failed to pin memory (ulimit too low?)\n");
	      munmap (addr, size);
	      return NULL;
	    }
	}

      return addr;
    }
  else
    return malloc (size);
}

static void *
linux_memspace_calloc (omp_memspace_handle_t memspace, size_t size, int pin)
{
  gomp_debug (0, "%s: memspace=%llu, size=%llu, pin=%d\n",
	      __FUNCTION__, (unsigned long long) memspace, (unsigned long long) size, pin);

  /* Explicit pinning may not be required.  */
  pin = pin && !always_pinned_mode;

  if (memspace == ompx_unified_shared_mem_space)
    {
      void *ret = gomp_usm_alloc (size);
      memset (ret, 0, size);
      return ret;
    }
  else if (pin)
    /* If PINned, 'linux_memspace_alloc' 'mmap's, which zero-initializes.  */
    return linux_memspace_alloc (memspace, size, pin);
  else
    return calloc (1, size);
}

static void
linux_memspace_free (omp_memspace_handle_t memspace, void *addr, size_t size,
		     int pin)
{
  gomp_debug (0, "%s: memspace=%llu, addr=%p, size=%llu, pin=%d\n",
	      __FUNCTION__, (unsigned long long) memspace, addr, (unsigned long long) size, pin);

  /* Explicit pinning may not be required.  */
  pin = pin && !always_pinned_mode;

  if (memspace == ompx_unified_shared_mem_space)
    gomp_usm_free (addr);
  else if (pin)
    {
      int using_device
	= __atomic_load_n (&using_device_for_register_page_locked,
			   MEMMODEL_RELAXED);
      gomp_debug (0, "  using_device=%d\n",
		  using_device);
      if (using_device == 1)
	gomp_unregister_page_locked (addr, size);
      else
	/* 'munlock'ing is implicit with following 'munmap'.  */
	;
      munmap (addr, size);
    }
  else
    free (addr);
}

static void *
linux_memspace_realloc (omp_memspace_handle_t memspace, void *addr,
			size_t oldsize, size_t size, int oldpin, int pin)
{
  gomp_debug (0, "%s: memspace=%llu, addr=%p, oldsize=%llu, size=%llu, oldpin=%d, pin=%d\n",
	      __FUNCTION__, (unsigned long long) memspace, addr, (unsigned long long) oldsize, (unsigned long long) size, oldpin, pin);

  /* Explicit pinning may not be required.  */
  pin = pin && !always_pinned_mode;

  if (memspace == ompx_unified_shared_mem_space)
    goto manual_realloc;
  else if (oldpin && pin)
    {
      /* We can only expect to be able to just 'mremap' if not using a device
	 for registering page-locked memory.  */
      int using_device
	= __atomic_load_n (&using_device_for_register_page_locked,
		       MEMMODEL_RELAXED);
      gomp_debug (0, "  using_device=%d\n",
		  using_device);
      if (using_device != 0)
	goto manual_realloc;

      gomp_debug (0, "  mremap\n");
      void *newaddr = mremap (addr, oldsize, size, MREMAP_MAYMOVE);
      if (newaddr == MAP_FAILED)
	return NULL;

      return newaddr;
    }
  else if (oldpin || pin)
    goto manual_realloc;
  else
    return realloc (addr, size);

manual_realloc:
  void *newaddr = linux_memspace_alloc (memspace, size, pin);
  if (newaddr)
    {
      memcpy (newaddr, addr, oldsize < size ? oldsize : size);
      linux_memspace_free (memspace, addr, oldsize, oldpin);
    }

  return newaddr;
}

#define MEMSPACE_ALLOC(MEMSPACE, SIZE, PIN) \
  linux_memspace_alloc (MEMSPACE, SIZE, PIN)
#define MEMSPACE_CALLOC(MEMSPACE, SIZE, PIN) \
  linux_memspace_calloc (MEMSPACE, SIZE, PIN)
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN) \
  linux_memspace_realloc (MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN)
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE, PIN) \
  linux_memspace_free (MEMSPACE, ADDR, SIZE, PIN)

#include "../../allocator.c"
