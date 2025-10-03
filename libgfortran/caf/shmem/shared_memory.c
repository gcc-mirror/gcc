/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "libgfortran.h"
#include "allocator.h"
#include "shared_memory.h"
#include "supervisor.h"

#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#elif defined(WIN32)
#include <Windows.h>
#include <Memoryapi.h>
#endif
#include <unistd.h>

/* This implements shared memory based on POSIX mmap.  We start with
   memory block of the size of the global shared memory data, rounded
   up to one pagesize, and enlarge as needed.

   We address the memory via a shared_memory_ptr, which is an offset into
   the shared memory block.  The metadata is situated at offset 0.

   In order to be able to resize the memory and to keep pointers
   valid, we keep the old mapping around, so the memory is actually
   visible several times to the process.  Thus, pointers returned by
   shared_memory_get_mem_with_alignment remain valid even when
   resizing.  */

static const char *ENV_PPID = "GFORTRAN_SHMEM_PPID";
static const char *ENV_BASE = "GFORTRAN_SHMEM_BASE";

void
shared_memory_set_env (pid_t pid)
{
#define bufsize 20
  char buffer[bufsize];

  snprintf (buffer, bufsize, "%d", pid);
#ifdef HAVE_SETENV
  setenv (ENV_PPID, buffer, 1);
#else
  SetEnvironmentVariable (ENV_PPID, buffer);
#endif
#undef bufsize
}

char *
shared_memory_get_env (void)
{
  return getenv (ENV_PPID);
}

/* Get a pointer into the shared memory block with alignemnt
   (works similar to sbrk).  */

shared_mem_ptr
shared_memory_get_mem_with_alignment (shared_memory_act *mem, size_t size,
				      size_t align)
{
  size_t aligned_curr_size = alignto (mem->glbl.meta->used, align);
  mem->glbl.meta->used = aligned_curr_size + size;
  return (shared_mem_ptr) {aligned_curr_size};
}

shared_mem_ptr
shared_memory_get_master (shared_memory_act *mem, size_t size, size_t align)
{
  if (mem->glbl.meta->master)
    return (shared_mem_ptr) {mem->glbl.meta->master};
  else
    {
      ptrdiff_t loc = mem->glbl.meta->used;
      shared_mem_ptr p
	= shared_memory_get_mem_with_alignment (mem, size, align);
      mem->glbl.meta->master = loc;
      return p;
    }
}

/* If another image changed the size, update the size accordingly.  */

void
shared_memory_prepare (shared_memory_act *)
{
  asm volatile ("" ::: "memory");
}

#define NAME_MAX 255

/* Initialize the memory with one page, the shared metadata of the
   shared memory is stored at the beginning.  */

void
shared_memory_init (shared_memory_act *mem, size_t size)
{
  char shm_name[NAME_MAX];
  const char *env_val = getenv (ENV_PPID), *base = getenv (ENV_BASE);
  pid_t ppid = getpid ();
  void *base_ptr;

  if (env_val)
    {
      int n = sscanf (env_val, "%d", &ppid);
      assert (n == 1);
    }
  snprintf (shm_name, NAME_MAX, "/gfor-shm-%d", ppid);
  if (base)
    {
      int n = sscanf (base, "%p", &base_ptr);
      assert (n == 1);
    }
  else
    base_ptr = NULL;

  if (!env_val)
    {
#ifdef HAVE_MMAP
      int res;

      mem->shm_fd = shm_open (shm_name, O_CREAT | O_RDWR | O_EXCL, 0600);
      if (mem->shm_fd == -1)
	{
	  perror ("creating shared memory segment failed.");
	  exit (1);
	}

      res = ftruncate (mem->shm_fd, size);
      if (res == -1)
	{
	  perror ("resizing shared memory segment failed.");
	  exit (1);
	}
#elif defined(WIN32)
      mem->shm_fd
	= CreateFileMapping (INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
			     size >> (sizeof (DWORD) * 8),
			     (DWORD) (size & ~((DWORD) 0)), shm_name);
      if (mem->shm_fd == NULL)
	{
	  LPVOID lpMsgBuf;
	  DWORD dw = GetLastError ();

	  if (FormatMessage (FORMAT_MESSAGE_ALLOCATE_BUFFER
			       | FORMAT_MESSAGE_FROM_SYSTEM
			       | FORMAT_MESSAGE_IGNORE_INSERTS,
			     NULL, dw,
			     MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
			     (LPTSTR) &lpMsgBuf, 0, NULL)
	      == 0)
	    {
	      fprintf (stderr, "formatting the error message failed.\n");
	      ExitProcess (dw);
	    }

	  fprintf (stderr, "creating shared memory segment failed: %d, %s\n",
		   dw, (LPCTSTR) lpMsgBuf);

	  LocalFree (lpMsgBuf);
	  exit (1);
	}
#else
#error "no way to map shared memory."
#endif
    }
  else
    {
#ifdef HAVE_MMAP
      mem->shm_fd = shm_open (shm_name, O_RDWR, 0600);
      if (mem->shm_fd == -1)
	{
	  perror ("opening shared memory segment failed.");
	  exit (1);
	}
#elif defined(WIN32)
      mem->shm_fd = OpenFileMapping (FILE_MAP_ALL_ACCESS, FALSE, shm_name);
      if (mem->shm_fd == NULL)
	{
	  perror ("opening shared memory segment failed.");
	  exit (1);
	}
#endif
    }
#ifdef HAVE_MMAP
  mem->glbl.base
    = mmap (base_ptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, mem->shm_fd, 0);
  if (base_ptr && mem->glbl.base != base_ptr)
    {
      /* The supervisor will start us again.  */
      close (mem->shm_fd);
      free (local);
      exit (210);
    }
  else if (!base_ptr && !mem->glbl.base)
    {
      perror ("mmap failed");
      exit (1);
    }
#elif defined(WIN32)
  mem->glbl.base
    = (LPTSTR) MapViewOfFileExNuma (mem->shm_fd, FILE_MAP_ALL_ACCESS, 0, 0,
				    size, base_ptr, NUMA_NO_PREFERRED_NODE);
  if (mem->glbl.base == NULL)
    {
      perror ("MapViewOfFile failed");
      exit (1);
    }
#endif
  if (!base_ptr)
    {
#define bufsize 20
      char buffer[bufsize];

      snprintf (buffer, bufsize, "%p", mem->glbl.base);
#ifdef HAVE_SETENV
      setenv (ENV_BASE, buffer, 1);
#else
      SetEnvironmentVariable (ENV_BASE, buffer);
#endif
#undef bufsize
    }
  mem->size = size;
  if (!env_val)
    *mem->glbl.meta
      = (global_shared_memory_meta) {sizeof (global_shared_memory_meta), 0};
}

void
shared_memory_cleanup (shared_memory_act *mem)
{
#ifdef HAVE_MMAP
  int res = munmap (mem->glbl.base, mem->size);
  if (res)
    {
      perror ("unmapping shared memory segment failed");
    }
  res = close (mem->shm_fd);
  if (res)
    {
      perror ("closing shm file handle failed. Trying to continue...");
    }
  if (this_image.image_num == -1)
    {
      char shm_name[NAME_MAX];

      snprintf (shm_name, NAME_MAX, "/gfor-shm-%s", shared_memory_get_env ());
      /* Only the supervisor is to delete the shm-file.  */
      res = shm_unlink (shm_name);
      if (res == -1)
	{
	  perror ("shm_unlink failed");
	  exit (1);
	}
    }
#elif defined(WIN32)
  if (!UnmapViewOfFile (mem->glbl.base))
    {
      perror ("unmapping shared memory segment failed");
    }
  CloseHandle (mem->shm_fd);
#endif
}
#undef NAME_MAX
