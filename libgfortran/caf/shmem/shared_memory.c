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

#include "libgfortran.h"
#include "allocator.h"
#include "shared_memory.h"

#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
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
  setenv (ENV_PPID, buffer, 1);
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
  int shm_fd, res;
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
      shm_fd = shm_open (shm_name, O_CREAT | O_RDWR | O_EXCL, 0600);
      if (shm_fd == -1)
	{
	  perror ("creating shared memory segment failed.");
	  exit (1);
	}

      res = ftruncate (shm_fd, size);
      if (res == -1)
	{
	  perror ("resizing shared memory segment failed.");
	  exit (1);
	}
    }
  else
    {
      shm_fd = shm_open (shm_name, O_RDWR, 0);
      if (shm_fd == -1)
	{
	  perror ("opening shared memory segment failed.");
	  exit (1);
	}
    }

  mem->glbl.base
    = mmap (base_ptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  res = close (shm_fd);
  if (mem->glbl.base == MAP_FAILED)
    {
      perror ("mmap failed");
      exit (1);
    }
  if (!base_ptr)
    {
#define bufsize 20
      char buffer[bufsize];

      snprintf (buffer, bufsize, "%p", mem->glbl.base);
      setenv (ENV_BASE, buffer, 1);
#undef bufsize
    }
  if (res)
    { // from close()
      perror ("closing shm file handle failed. Trying to continue...");
    }
  mem->size = size;
  if (!env_val)
    *mem->glbl.meta
      = (global_shared_memory_meta) {sizeof (global_shared_memory_meta), 0};

}

void
shared_memory_cleanup (shared_memory_act *)
{
  char shm_name[NAME_MAX];
  int res;

  snprintf (shm_name, NAME_MAX, "/gfor-shm-%s", shared_memory_get_env ());
  res = shm_unlink (shm_name);
  if (res == -1)
    {
      perror ("shm_unlink failed");
      exit (1);
    }
}
#undef NAME_MAX
