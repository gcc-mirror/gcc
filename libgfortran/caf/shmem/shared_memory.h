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

#ifndef SHARED_MEMORY_H
#define SHARED_MEMORY_H

#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>

/* Global metadata for shared memory, always kept at offset 0.  */

typedef struct
{
  size_t used;
  ptrdiff_t master;
} global_shared_memory_meta;

/* Type realization for shared_memory.  */

typedef struct shared_memory_act
{
  union
  {
    void *base;
    global_shared_memory_meta *meta;
  } glbl;
  size_t size; // const
} shared_memory_act;

/* A struct to serve as shared memory object.  */

typedef struct shared_memory_act * shared_memory;

#define SHMPTR_NULL ((shared_mem_ptr) {.offset = 0})
#define SHMPTR_IS_NULL(x) (x.offset == 0)

#define SHMPTR_DEREF(x, s, sm) ((x) = *(__typeof (x) *) s.p)
#define SHMPTR_AS(type, s, sm) ((type) (*((void **) sm) + s.offset))
#define AS_SHMPTR(p, sm) ((shared_mem_ptr) {.offset = (p) - sm.glbl.base})

#define SHARED_MEMORY_RAW_ALLOC(mem, t, n)                                     \
  shared_memory_get_mem_with_alignment (mem, sizeof (t) * n, __alignof__ (t))

#define SHARED_MEMORY_RAW_ALLOC_PTR(mem, t) \
  SHMPTR_AS (t *, SHARED_MEMORY_RAW_ALLOC (mem, t, 1), mem)

/* A shared-memory pointer is implemented as an offset into the shared
   memory region.  */

typedef struct shared_mem_ptr
{
  ptrdiff_t offset;
} shared_mem_ptr;

void shared_memory_init (shared_memory, size_t);

void shared_memory_cleanup (shared_memory);

void shared_memory_prepare (shared_memory);

shared_mem_ptr shared_memory_get_mem_with_alignment (shared_memory mem,
						     size_t size, size_t align);

shared_mem_ptr shared_memory_get_master (shared_memory pmem, size_t size,
					 size_t align);

void shared_memory_set_env (pid_t pid);

char *shared_memory_get_env (void);

#endif
