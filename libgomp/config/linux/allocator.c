/* Copyright (C) 2022-2025 Free Software Foundation, Inc.
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
#define LIBGOMP_USE_LIBNUMA
#endif

/* Implement malloc routines that can handle pinned memory on Linux.
   
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
#include "libgomp.h"
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIu64.  */
#endif

static void *
linux_memspace_alloc (omp_memspace_handle_t memspace, size_t size, int pin)
{
  (void)memspace;

  if (pin)
    {
      /* Note that mmap always returns zeroed memory and is therefore also a
	 suitable implementation of calloc.  */
      void *addr = mmap (NULL, size, PROT_READ | PROT_WRITE,
			 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
      if (addr == MAP_FAILED)
	return NULL;

      if (mlock (addr, size))
	{
#ifdef HAVE_INTTYPES_H
	  gomp_debug (0, "libgomp: failed to pin %"PRIu64" bytes of"
		      " memory (ulimit too low?)\n", (uint64_t) size);
#else
	  gomp_debug (0, "libgomp: failed to pin %lu bytes of"
		      " memory (ulimit too low?)\n", (unsigned long) size);
#endif
	  munmap (addr, size);
	  return NULL;
	}

      return addr;
    }
  else
    return malloc (size);
}

static void *
linux_memspace_calloc (omp_memspace_handle_t memspace, size_t size, int pin)
{
  if (pin)
    return linux_memspace_alloc (memspace, size, pin);
  else
    return calloc (1, size);
}

static void
linux_memspace_free (omp_memspace_handle_t memspace, void *addr, size_t size,
		     int pin)
{
  (void)memspace;

  if (pin)
    munmap (addr, size);
  else
    free (addr);
}

static void *
linux_memspace_realloc (omp_memspace_handle_t memspace, void *addr,
			size_t oldsize, size_t size, int oldpin, int pin)
{
  if (oldpin && pin)
    {
      void *newaddr = mremap (addr, oldsize, size, MREMAP_MAYMOVE);
      if (newaddr == MAP_FAILED)
	return NULL;

      return newaddr;
    }
  else if (oldpin || pin)
    {
      void *newaddr = linux_memspace_alloc (memspace, size, pin);
      if (newaddr)
	{
	  memcpy (newaddr, addr, oldsize < size ? oldsize : size);
	  linux_memspace_free (memspace, addr, oldsize, oldpin);
	}

      return newaddr;
    }
  else
    return realloc (addr, size);
}

static int
linux_memspace_validate (omp_memspace_handle_t, unsigned, int)
{
  /* Everything should be accepted on Linux, including pinning.  */
  return 1;
}

#define MEMSPACE_ALLOC(MEMSPACE, SIZE, PIN) \
  linux_memspace_alloc (MEMSPACE, SIZE, PIN)
#define MEMSPACE_CALLOC(MEMSPACE, SIZE, PIN) \
  linux_memspace_calloc (MEMSPACE, SIZE, PIN)
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN) \
  linux_memspace_realloc (MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN)
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE, PIN) \
  linux_memspace_free (MEMSPACE, ADDR, SIZE, PIN)
#define MEMSPACE_VALIDATE(MEMSPACE, ACCESS, PIN) \
  linux_memspace_validate (MEMSPACE, ACCESS, PIN)

#include "../../allocator.c"
