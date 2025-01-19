/* Copyright (C) 2023-2025 Free Software Foundation, Inc.

   Contributed by Siemens.

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

#include <assert.h>
#include <string.h>
#include "libgomp.h"

struct indirect_map_t
{
  void *host_addr;
  void *target_addr;
};

typedef struct indirect_map_t *hash_entry_type;

static inline void * htab_alloc (size_t size) { return gomp_malloc (size); }
static inline void htab_free (void *ptr) { free (ptr); }

#include "hashtab.h"

static inline hashval_t
htab_hash (hash_entry_type element)
{
  return hash_pointer (element->host_addr);
}

static inline bool
htab_eq (hash_entry_type x, hash_entry_type y)
{
  return x->host_addr == y->host_addr;
}

void **GOMP_INDIRECT_ADDR_MAP = NULL;

/* Use a hashtab to lookup the target address instead of using a linear
   search.  */
#define USE_HASHTAB_LOOKUP

#ifdef USE_HASHTAB_LOOKUP

static htab_t indirect_htab = NULL;

/* Build the hashtab used for host->target address lookups.  */

void
build_indirect_map (void)
{
  size_t num_ind_funcs = 0;
  void **map_entry;

  if (!GOMP_INDIRECT_ADDR_MAP)
    return;

  if (!indirect_htab)
    {
      /* Count the number of entries in the NULL-terminated address map.  */
      for (map_entry = GOMP_INDIRECT_ADDR_MAP; *map_entry;
	   map_entry += 2, num_ind_funcs++);

      /* Build hashtab for address lookup.  */
      indirect_htab = htab_create (num_ind_funcs);
      map_entry = GOMP_INDIRECT_ADDR_MAP;

      for (int i = 0; i < num_ind_funcs; i++, map_entry += 2)
	{
	  struct indirect_map_t element = { *map_entry, NULL };
	  hash_entry_type *slot = htab_find_slot (&indirect_htab, &element,
						  INSERT);
	  *slot = (hash_entry_type) map_entry;
	}
    }
}

void *
GOMP_target_map_indirect_ptr (void *ptr)
{
  /* NULL pointers always resolve to NULL.  */
  if (!ptr)
    return ptr;

  assert (indirect_htab);

  struct indirect_map_t element = { ptr, NULL };
  hash_entry_type entry = htab_find (indirect_htab, &element);
  return entry ? entry->target_addr : ptr;
}

#else

void
build_indirect_map (void)
{
}

void *
GOMP_target_map_indirect_ptr (void *ptr)
{
  /* NULL pointers always resolve to NULL.  */
  if (!ptr)
    return ptr;

  assert (GOMP_INDIRECT_ADDR_MAP);

  for (void **map_entry = GOMP_INDIRECT_ADDR_MAP; *map_entry;
       map_entry += 2)
    if (*map_entry == ptr)
      return (void *) *(map_entry + 1);

  return ptr;
}

#endif
