/* Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include "libgomp.h"

void *GOMP_INDIRECT_ADDR_MAP = NULL;

#define USE_HASHTAB_LOOKUP

#ifdef USE_HASHTAB_LOOKUP

#include <string.h>  /* For memset.  */

/* Use a hashtab to lookup the target address instead of using a linear
   search.

   With newer libgomp on the host the hash is already initialized on the host
   (i.e plugin/plugin-gcn.c). Thus, build_indirect_map is only used as
   fallback with older glibc.  */

void *GOMP_INDIRECT_ADDR_HMAP = NULL;

typedef unsigned __int128 hash_entry_type;
#define INDIRECT_DEV_ADDR(p) ((void*) (uintptr_t) (p >> 64))
#define INDIRECT_HOST_ADDR(p) ((void *) (uintptr_t) p)
#define SET_INDIRECT_HOST_ADDR(p, host) p = (((unsigned __int128) (uintptr_t) host))
#define SET_INDIRECT_ADDRS(p, h, d) \
  p = (((unsigned __int128) h) + (((unsigned __int128) d) << 64))

/* Besides the sizes, also the endianness either needs to agree or
   host-device memcpy needs to take care of this.  */
_Static_assert (sizeof (unsigned __int128) == 2*sizeof(void*),
		"indirect_target_map_t size mismatch");

static inline void * htab_alloc (size_t size) { return gomp_malloc (size); }
static inline void htab_free (void *ptr) { __builtin_unreachable (); }

#include "hashtab.h"

static inline hashval_t
htab_hash (hash_entry_type element)
{
  return hash_pointer (INDIRECT_HOST_ADDR (element));
}

static inline bool
htab_eq (hash_entry_type x, hash_entry_type y)
{
  return INDIRECT_HOST_ADDR (x) == INDIRECT_HOST_ADDR (y);
}

void *
GOMP_target_map_indirect_ptr (void *ptr)
{
  /* NULL pointers always resolve to NULL.  */
  if (!ptr)
    return ptr;

  assert (GOMP_INDIRECT_ADDR_HMAP);

  hash_entry_type element;
  SET_INDIRECT_HOST_ADDR (element, ptr);
  hash_entry_type entry = htab_find ((htab_t) GOMP_INDIRECT_ADDR_HMAP, element);
  return entry ? INDIRECT_DEV_ADDR (entry) : ptr;
}

/* Build the hashtab used for host->target address lookups.  */

void
build_indirect_map (void)
{
  size_t num_ind_funcs = 0;
  uint64_t *map_entry;

  if (!GOMP_INDIRECT_ADDR_MAP || GOMP_INDIRECT_ADDR_HMAP)
    return;

  /* Count the number of entries in the NULL-terminated address map.  */
  for (map_entry = (uint64_t *) GOMP_INDIRECT_ADDR_MAP; *map_entry;
    map_entry += 2, num_ind_funcs++);

  /* Build hashtab for address lookup.  */
  htab_t indirect_htab = htab_create (num_ind_funcs);
  GOMP_INDIRECT_ADDR_HMAP = (void *) indirect_htab;

  map_entry = GOMP_INDIRECT_ADDR_MAP;
  for (int i = 0; i < num_ind_funcs; i++, map_entry += 2)
    {
      hash_entry_type element;
      SET_INDIRECT_ADDRS (element, *map_entry, *(map_entry + 1));
      hash_entry_type *slot = htab_find_slot (&indirect_htab, element,
					      INSERT);
      *slot = element;
    }
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
