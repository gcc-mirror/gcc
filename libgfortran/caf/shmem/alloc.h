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

#ifndef ALLOC_H
#define ALLOC_H

#include "allocator.h"
#include "hashmap.h"

/* High-level interface for shared memory allocation.
   Handle allocation and freeing of blocks in the shared memory for coarrays.
   While allocator keeps track of allocated and freeed portions, this "class"
   allows allocation of coarrays identified by a memid and associate them
   across images.
 */

/* The part of the alloc interface being shared with all other images.  There
   must be only one of these objects!  */
typedef struct alloc_shared
{
  allocator_shared allocator_s;
} alloc_shared;

/* This is the image's local part of the alloc interface.  */

typedef struct alloc
{
  alloc_shared *as;
  shared_memory mem;
  allocator alloc;
  hashmap hm;
} alloc;

/* Initialize the local instance of the alloc interface.  This routine is to be
   called by every worker image and NOT by the supervisor.  */
void alloc_init (alloc *, shared_memory);

/* The routine MUST ONLY called by the supervisor process.
   Initialize the shared part of the alloc interface.  The local one is only
   initialized to be able to pass it to the other components needing it.  */
void alloc_init_supervisor (alloc *, shared_memory);

/* Get a shared memory block identified by id, or a new one, when the id
   is not known yet.  This routine locks the allocator lock itself.  */
void *alloc_get_memory_by_id (alloc *, size_t, memid);

/* Same as alloc_get_memory_by_id, but it does not lock the allocator lock and
   returns an additional bool, that is true, when the memory has been allocated
   freshly.  */
void *alloc_get_memory_by_id_created (alloc *, size_t, memid, bool *);

/* Mark the memory identified by id as free.  This reduces the use counter on
   the memory and sets is free, when the count goes to zero.  */
void alloc_free_memory_with_id (alloc *, memid);

/* Get the allocator for reuse in other interfaces.  */
allocator *alloc_get_allocator (alloc *);

#endif
