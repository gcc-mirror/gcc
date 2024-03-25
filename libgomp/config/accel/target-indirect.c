/* Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#define splay_tree_prefix indirect
#define splay_tree_c
#include "splay-tree.h"

volatile void **GOMP_INDIRECT_ADDR_MAP = NULL;

/* Use a splay tree to lookup the target address instead of using a
   linear search.  */
#define USE_SPLAY_TREE_LOOKUP

#ifdef USE_SPLAY_TREE_LOOKUP

static struct indirect_splay_tree_s indirect_map;
static indirect_splay_tree_node indirect_array = NULL;

/* Build the splay tree used for host->target address lookups.  */

void
build_indirect_map (void)
{
  size_t num_ind_funcs = 0;
  volatile void **map_entry;
  static int lock = 0; /* == gomp_mutex_t lock; gomp_mutex_init (&lock); */

  if (!GOMP_INDIRECT_ADDR_MAP)
    return;

  gomp_mutex_lock (&lock);

  if (!indirect_array)
    {
      /* Count the number of entries in the NULL-terminated address map.  */
      for (map_entry = GOMP_INDIRECT_ADDR_MAP; *map_entry;
	   map_entry += 2, num_ind_funcs++);

      /* Build splay tree for address lookup.  */
      indirect_array = gomp_malloc (num_ind_funcs * sizeof (*indirect_array));
      indirect_splay_tree_node array = indirect_array;
      map_entry = GOMP_INDIRECT_ADDR_MAP;

      for (int i = 0; i < num_ind_funcs; i++, array++)
	{
	  indirect_splay_tree_key k = &array->key;
	  k->host_addr = (uint64_t) *map_entry++;
	  k->target_addr = (uint64_t) *map_entry++;
	  array->left = NULL;
	  array->right = NULL;
	  indirect_splay_tree_insert (&indirect_map, array);
	}
    }

  gomp_mutex_unlock (&lock);
}

void *
GOMP_target_map_indirect_ptr (void *ptr)
{
  /* NULL pointers always resolve to NULL.  */
  if (!ptr)
    return ptr;

  assert (indirect_array);

  struct indirect_splay_tree_key_s k;
  indirect_splay_tree_key node = NULL;

  k.host_addr = (uint64_t) ptr;
  node = indirect_splay_tree_lookup (&indirect_map, &k);

  return node ? (void *) node->target_addr : ptr;
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

  for (volatile void **map_entry = GOMP_INDIRECT_ADDR_MAP; *map_entry;
       map_entry += 2)
    if (*map_entry == ptr)
      return (void *) *(map_entry + 1);

  return ptr;
}

#endif
