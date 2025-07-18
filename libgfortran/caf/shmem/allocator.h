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

/* A malloc() - and free() - like interface, but for shared memory
   pointers, except that we pass the size to free as well.  */

#ifndef ALLOCATOR_HDR
#define ALLOCATOR_HDR

#include "shared_memory.h"

#include <stddef.h>
#include <pthread.h>

/* The number of bits a void pointer has.  */
#define VOIDP_BITS (__CHAR_BIT__ * sizeof (void *))

/* The shared memory part of the allocator.  */
typedef struct {
  pthread_mutex_t lock;
  shared_mem_ptr free_bucket_head[VOIDP_BITS];
} allocator_shared;

/* The image local part of the allocator.  */
typedef struct {
  allocator_shared *s;
  shared_memory shm;
} allocator;

/* The size of a page on this architecture.  */
extern size_t pagesize;

/* Helper routine to align a size to a given boundary.  */
size_t alignto (size_t, size_t);

/* Helper routine to round a size to multiple of the architecture's pagesize.
 */
size_t round_to_pagesize (size_t);

/* Link the worker's allocator with the part in the shared memory.  */
void allocator_init (allocator *, allocator_shared *, shared_memory);

/* Initialize the allocator.  This MUST be called ONLY be the supervisor and
   only once!  */
void allocator_init_supervisor (allocator *, allocator_shared *, shared_memory);

/* Request a block of shared memory.  The memory is not linked with the other
   images.  The shared_mem_ptr returned is only local to the calling image.
   When requiring a memory block shared between all images, call
   alloc_get_memory_by_id...().  */
shared_mem_ptr allocator_shared_malloc (allocator *, size_t size);

/* Free the given piece of memory.  This routine just inserts the memory chunk
   into the bucket list of free memory.  It does not join adjacent blocks of
   memory (not implemented yet).  */
void allocator_shared_free (allocator *, shared_mem_ptr, size_t size);

/* Lock the allocator lock preventing any image from modifying memory management
   structures.  Do not forget to unlock.  This interface is exposed to be able
   to do more then just get the memory without having to introduce a second lock
   and the problems with having to get both.  */
void allocator_lock (allocator *);

/* Unlock the allocator lock.  */
void allocator_unlock (allocator *);

#endif
