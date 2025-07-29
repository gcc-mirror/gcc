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

/* This provides the coarray-specific features (like IDs etc) for
   allocator.c, in turn calling routines from shared_memory.c.
*/

#include "alloc.h"
#include "../caf_error.h"
#include "supervisor.h"
#include "shared_memory.h"

#include <assert.h>
#include <pthread.h>
#include <string.h>

/* Worker's part to initialize the alloc interface.  */

void
alloc_init (alloc *iface, shared_memory mem)
{
  iface->as = &this_image.supervisor->alloc_shared;
  iface->mem = mem;
  allocator_init (&iface->alloc, &iface->as->allocator_s, mem);
  hashmap_init (&iface->hm, &this_image.supervisor->hms, &iface->alloc);
}

/* Allocate the shared memory interface.  This is called before we have
   multiple images.  Called only by supervisor.  */

void
alloc_init_supervisor (alloc *iface, shared_memory mem)
{
  iface->as = &this_image.supervisor->alloc_shared;
  iface->mem = mem;
  allocator_init_supervisor (&iface->alloc, &iface->as->allocator_s, mem);
  hashmap_init_supervisor (&iface->hm, &this_image.supervisor->hms,
			   &iface->alloc);
}

/* Return a local pointer into a shared memory object identified by
   id.  If the object is already found, it has been allocated before,
   so just increase the reference counter.

   The pointers returned by this function remain valid even if the
   size of the memory allocation changes (see shared_memory.c).  */

static void *
get_memory_by_id_internal (alloc *iface, size_t size, memid id, bool *created)
{
  hashmap_search_result res;
  shared_mem_ptr shared_ptr;
  void *ret;

  shared_memory_prepare (iface->mem);

  res = hashmap_get (&iface->hm, id);

  if (hm_search_result_contains (&res))
    {
      size_t found_size;
      found_size = hm_search_result_size (&res);
      if (found_size < size)
	{
	  allocator_unlock (&iface->alloc);
	  caf_runtime_error (
	    "Size mismatch for coarray allocation id %zd: found = %lu "
	    "< size = %lu\n",
	    id, found_size, size);
	  return NULL; // The runtime_error exit()s, so this is never reached.
	}
      shared_ptr = hm_search_result_ptr (&res);
      hashmap_inc (&iface->hm, id, &res);

      if (created)
	*created = false;
      ret = SHMPTR_AS (void *, shared_ptr, iface->mem);
    }
  else
    {
      shared_ptr = allocator_shared_malloc (&iface->alloc, size);
      hashmap_set (&iface->hm, id, NULL, shared_ptr, size);

      if (created)
	*created = true;

      ret = SHMPTR_AS (void *, shared_ptr, iface->mem);
    }

  return ret;
}

void *
alloc_get_memory_by_id (alloc *iface, size_t size, memid id)
{
  allocator_lock (&iface->alloc);
  void *ret = get_memory_by_id_internal (iface, size, id, NULL);
  allocator_unlock (&iface->alloc);
  return ret;
}

void *
alloc_get_memory_by_id_created (alloc *iface, size_t size, memid id,
				bool *created)
{
  return get_memory_by_id_internal (iface, size, id, created);
}


/* Free memory with id.  Free it if this is the last image which
   holds that memory segment, decrease the reference count otherwise.  */

void
alloc_free_memory_with_id (alloc *iface, memid id)
{
  hashmap_search_result res;
  int entries_left;

  allocator_lock (&iface->alloc);
  shared_memory_prepare (iface->mem);

  res = hashmap_get (&iface->hm, id);
  if (!hm_search_result_contains (&res))
    {
      allocator_unlock (&iface->alloc);
      caf_runtime_error ("Error in free_memory_with_id: %zd not found.\n", id);
      return;
    }

  entries_left = hashmap_dec (&iface->hm, id, &res);
  assert (entries_left >= 0);

  if (entries_left == 0)
    {
      allocator_shared_free (&iface->alloc, hm_search_result_ptr (&res),
			     hm_search_result_size (&res));
    }

  allocator_unlock (&iface->alloc);
  return;
}

allocator *
alloc_get_allocator (alloc *iface)
{
  return &iface->alloc;
}
