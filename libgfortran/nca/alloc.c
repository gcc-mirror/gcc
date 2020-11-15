/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Thomas Koenig

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

/* This provides the coarray-specific features (like IDs etc) for
   allocator.c, in turn calling routines from shared_memory.c.
*/

#include "libgfortran.h"
#include "shared_memory.h"
#include "allocator.h"
#include "hashmap.h"
#include "alloc.h"

#include <string.h>

/* Return a local pointer into a shared memory object identified by
   id.  If the object is already found, it has been allocated before,
   so just increase the reference counter.

   The pointers returned by this function remain valid even if the
   size of the memory allocation changes (see shared_memory.c).  */

static void *
get_memory_by_id_internal (alloc_iface *iface, size_t size, memid id,
			   bool zero_mem)
{
  hashmap_search_result res;
  shared_mem_ptr shared_ptr;
  void *ret;

  pthread_mutex_lock (&iface->as->lock);
  shared_memory_prepare (iface->mem);

  res = hashmap_get (&iface->hm, id);

  if (hm_search_result_contains (&res))
    {
      size_t found_size;
      found_size = hm_search_result_size (&res);
      if (found_size != size)
	{
	  dprintf (2,
		   "Size mismatch for coarray allocation id %p: found = %lu "
		   "!= size = %lu\n",
		   (void *)id, found_size, size);
	  pthread_mutex_unlock (&iface->as->lock);
	  exit (1);
	}
      shared_ptr = hm_search_result_ptr (&res);
      hashmap_inc (&iface->hm, id, &res);
    }
  else
    {
      shared_ptr = shared_malloc (&iface->alloc, size);
      hashmap_set (&iface->hm, id, NULL, shared_ptr, size);
    }

  ret = SHMPTR_AS (void *, shared_ptr, iface->mem);
  if (zero_mem)
    memset (ret, '\0', size);

  pthread_mutex_unlock (&iface->as->lock);
  return ret;
}

void *
get_memory_by_id (alloc_iface *iface, size_t size, memid id)
{
  return get_memory_by_id_internal (iface, size, id, 0);
}

void *
get_memory_by_id_zero (alloc_iface *iface, size_t size, memid id)
{
  return get_memory_by_id_internal (iface, size, id, 1);
}

/* Free memory with id.  Free it if this is the last image which
   holds that memory segment, decrease the reference count otherwise.  */

void
free_memory_with_id (alloc_iface *iface, memid id)
{
  hashmap_search_result res;
  int entries_left;

  pthread_mutex_lock (&iface->as->lock);
  shared_memory_prepare (iface->mem);

  res = hashmap_get (&iface->hm, id);
  if (!hm_search_result_contains (&res))
    {
      pthread_mutex_unlock (&iface->as->lock);
      char buffer[100];
      snprintf (buffer, sizeof (buffer),
		"Error in free_memory_with_id: %p not found", (void *)id);
      /* FIXME: For some reason, internal_error (NULL, buffer) fails to link,
       * so we use dprintf at the moment.  */
      dprintf (2, buffer);
      exit (1);
    }

  entries_left = hashmap_dec (&iface->hm, id, &res);
  assert (entries_left >= 0);

  if (entries_left == 0)
    {
      shared_free (&iface->alloc, hm_search_result_ptr (&res),
		   hm_search_result_size (&res));
    }

  pthread_mutex_unlock (&iface->as->lock);
  return;
}

/* Allocate the shared memory interface. This is called before we have
   multiple images.  */

void
alloc_iface_init (alloc_iface *iface, shared_memory *mem)
{

  iface->as = SHARED_MEMORY_RAW_ALLOC_PTR (mem, alloc_iface_shared);
  iface->mem = mem;
  initialize_shared_mutex (&iface->as->lock);
  allocator_init (&iface->alloc, &iface->as->allocator_s, mem);
  hashmap_init (&iface->hm, &iface->as->hms, &iface->alloc, mem);
}

allocator *
get_allocator (alloc_iface *iface)
{
  return &iface->alloc;
}
