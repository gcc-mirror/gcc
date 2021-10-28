/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

This file is part of the GNU Fortran Native Coarray Library (libnca).

Libnca is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libnca is distributed in the hope that it will be useful,
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
#include "libcoarraynative.h"

#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

/* This implements shared memory based on POSIX mmap.  We start with
   memory block of the size of the global shared memory data, rounded
   up to one pagesize, and enlarge as needed.

   We address the memory via a shared_memory_ptr, which is an offset into
   the shared memory block. The metadata is situated at offset 0.

   In order to be able to resize the memory and to keep pointers
   valid, we keep the old mapping around, so the memory is actually
   visible several times to the process.  Thus, pointers returned by
   shared_memory_get_mem_with_alignment remain valid even when
   resizing.  */

/* Global metadata for shared memory, always kept at offset 0.  */

typedef struct
{
  size_t used;
} global_shared_memory_meta;

/* Type realization for opaque type shared_memory.  */

typedef struct shared_memory_act
{
  union { 
    void *base; 
    global_shared_memory_meta *meta;
  } glbl;
  size_t size; // const
} shared_memory_act;

/* Convenience wrapper for mmap.  */

static inline void *
map_memory (size_t size)
{
  void *ret
      = mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
  if (ret == MAP_FAILED)
    {
      perror ("mmap failed");
      exit (1);
    }
  return ret;
}


/* Get a pointer into the shared memory block with alignemnt
   (works similar to sbrk).  */

shared_mem_ptr
shared_memory_get_mem_with_alignment (shared_memory_act **pmem, size_t size,
				      size_t align)
{
  shared_memory_act* mem = *pmem;
  size_t aligned_curr_size = alignto(mem->glbl.meta->used, align);
  void *p = mem->glbl.base+aligned_curr_size;
  mem->glbl.meta->used = aligned_curr_size + size;
  return (shared_mem_ptr) {.p = p};
}

/* If another image changed the size, update the size accordingly.  */

void
shared_memory_prepare (shared_memory_act **pmem)
{
  asm volatile("":::"memory");
}

/* Initialize the memory with one page, the shared metadata of the
   shared memory is stored at the beginning.  */

void
shared_memory_init (shared_memory_act **pmem, size_t size)
{
  shared_memory_act *mem;

  mem = malloc (sizeof(shared_memory_act));
  mem->glbl.base = map_memory(size);
  mem->size = size;
  mem->glbl.meta->used = sizeof(global_shared_memory_meta);

  *pmem = mem;
}
