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

/* A malloc() - and free() - like interface, but for shared memory
   pointers, except that we pass the size to free as well.  */

#include "libgfortran.h"
#include "shared_memory.h"
#include "allocator.h"

typedef struct {
  shared_mem_ptr next;
} bucket;

/* Initialize the allocator.  */

void 
allocator_init (allocator *a, allocator_shared *s, shared_memory *sm)
{
  a->s = s;
  a->shm = sm;
  for (int i = 0; i < PTR_BITS; i++)
    s->free_bucket_head[i] = SHMPTR_NULL;
}

/* Main allocation routine, works like malloc.  Round up allocations
   to the next power of two and keep free lists in buckets.  */

#define MAX_ALIGN 16

shared_mem_ptr 
shared_malloc (allocator *a, size_t size)
{
  shared_mem_ptr ret;
  size_t sz;
  size_t act_size;
  int bucket_list_index;

  sz = next_power_of_two (size);
  act_size = sz > sizeof (bucket) ? sz : sizeof (bucket);
  bucket_list_index = __builtin_clzl(act_size);

  if (SHMPTR_IS_NULL (a->s->free_bucket_head[bucket_list_index]))
    return shared_memory_get_mem_with_alignment (a->shm, act_size, MAX_ALIGN);

  ret = a->s->free_bucket_head[bucket_list_index];
  a->s->free_bucket_head[bucket_list_index]
    = (SHMPTR_AS (bucket *, ret, a->shm)->next);
  assert(ret.offset != 0);
  return ret;
}

/* Free memory.  */

void
shared_free (allocator *a, shared_mem_ptr p, size_t size) {
  bucket *b;
  size_t sz;
  int bucket_list_index;
  size_t act_size;

  sz = next_power_of_two (size);
  act_size = sz > sizeof (bucket) ? sz : sizeof (bucket);
  bucket_list_index = __builtin_clzl(act_size);

  b = SHMPTR_AS(bucket *, p, a->shm);
  b->next = a->s->free_bucket_head[bucket_list_index];
  a->s->free_bucket_head[bucket_list_index] = p;
}
