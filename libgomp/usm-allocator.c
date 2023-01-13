/* Copyright (C) 2023 Free Software Foundation, Inc.

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

/* This is a simple "malloc" implementation intended for use with Unified
   Shared Memory.  It allocates memory from a pool allocated and configured
   by the device plugin, and is intended to be #included from there.  It
   keeps the allocated/free chain in a side-table (splay tree) to ensure that
   the allocation routine does not migrate all the USM pages back into host
   memory.  */


#include "libgomp.h"

/* Use a splay tree to track USM allocations.  */

typedef struct usm_splay_tree_node_s *usm_splay_tree_node;
typedef struct usm_splay_tree_s *usm_splay_tree;
typedef struct usm_splay_tree_key_s *usm_splay_tree_key;

struct usm_splay_tree_key_s {
  void *base;
  size_t size;
};

static inline int
usm_splay_compare (usm_splay_tree_key x, usm_splay_tree_key y)
{
  return (x->base == y->base ? 0
	  : x->base > y->base ? 1
	  : -1);
}
#define splay_tree_prefix usm
#include "splay-tree.h"

static int usm_lock = 0;
static struct usm_splay_tree_s usm_allocations = { NULL };
static struct usm_splay_tree_s usm_free_space = { NULL };

/* 128-byte granularity means GPU cache-line aligned.  */
#define ALIGN(VAR) (((VAR) + 127) & ~127)

/* Coalesce contiguous free space into one entry.  This considers the entries
   either side of the root node only, so it should be called each time a new
   entry in inserted into the root.  */

static void
usm_coalesce_free_space ()
{
  usm_splay_tree_node prev, next, node = usm_free_space.root;

  for (prev = node->left; prev && prev->right; prev = prev->right)
    ;
  for (next = node->right; next && next->left; next = next->left)
    ;

  /* Coalesce adjacent free chunks.  */
  if (next
      && node->key.base + node->key.size == next->key.base)
    {
      /* Free chunk follows.  */
      node->key.size += next->key.size;
      usm_splay_tree_remove (&usm_free_space, &next->key);
      free (next);
    }
  if (prev
      && prev->key.base + prev->key.size == node->key.base)
    {
      /* Free chunk precedes.  */
      prev->key.size += node->key.size;
      usm_splay_tree_remove (&usm_free_space, &node->key);
      free (node);
    }
}

/* Add a new memory region into the free chain.  This is how the USM heap is
   initialized and extended.  If the new region is contiguous with an existing
   region then any free space will be coalesced.  */

static void
usm_register_memory (char *base, size_t size)
{
  if (base == NULL)
    return;

  while (__atomic_exchange_n (&usm_lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  usm_splay_tree_node node = malloc (sizeof (struct usm_splay_tree_node_s));
  node->key.base = base;
  node->key.size = size;
  node->left = NULL;
  node->right = NULL;
  usm_splay_tree_insert (&usm_free_space, node);
  usm_coalesce_free_space (node);

  __atomic_store_n (&usm_lock, 0, MEMMODEL_RELEASE);
}

/* This splay_tree_foreach callback selects the first free space large enough
   to hold the allocation needed.  Since the splay_tree walk may start in the
   middle the "first" isn't necessarily the "leftmost" entry.  */

struct usm_callback_data {
  size_t size;
  usm_splay_tree_node found;
};

static int
usm_alloc_callback (usm_splay_tree_key key, void *data)
{
  struct usm_callback_data *cbd = (struct usm_callback_data *)data;

  if (key->size >= cbd->size)
    {
      cbd->found = (usm_splay_tree_node)key;
      return 1;
    }

  return 0;
}

/* USM "malloc".  Selects and moves and address range from usm_free_space to
   usm_allocations, while leaving any excess in usm_free_space.  */

static void *
usm_alloc (size_t size)
{
  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  /* Acquire the lock.  */
  while (__atomic_exchange_n (&usm_lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  if (!usm_free_space.root)
    {
      /* No memory registered, or no free space.  */
      __atomic_store_n (&usm_lock, 0, MEMMODEL_RELEASE);
      return NULL;
    }

  /* Find a suitable free block.  */
  struct usm_callback_data cbd = {size, NULL};
  usm_splay_tree_foreach_lazy (&usm_free_space, usm_alloc_callback, &cbd);
  usm_splay_tree_node freenode = cbd.found;

  void *result = NULL;
  if (freenode)
    {
      /* Allocation successful.  */
      result = freenode->key.base;
      usm_splay_tree_node allocnode = malloc (sizeof (*allocnode));
      allocnode->key.base = result;
      allocnode->key.size = size;
      allocnode->left = NULL;
      allocnode->right = NULL;
      usm_splay_tree_insert (&usm_allocations, allocnode);

      /* Update the free chain.  */
      size_t stillfree_size = freenode->key.size - size;
      if (stillfree_size > 0)
	{
	  freenode->key.base = freenode->key.base + size;
	  freenode->key.size = stillfree_size;
	}
      else
	{
	  usm_splay_tree_remove (&usm_free_space, &freenode->key);
	  free (freenode);
	}
    }

  /* Release the lock.  */
  __atomic_store_n (&usm_lock, 0, MEMMODEL_RELEASE);

  return result;
}

/* USM "free".  Moves an address range from usm_allocations to usm_free_space
   and merges that record with any contiguous free memory.  */

static void
usm_free (void *addr)
{
  /* Acquire the lock.  */
  while (__atomic_exchange_n (&usm_lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  /* Convert the memory map to free.  */
  struct usm_splay_tree_key_s key = {addr};
  usm_splay_tree_key found = usm_splay_tree_lookup (&usm_allocations, &key);
  if (!found)
    GOMP_PLUGIN_fatal ("invalid free");
  usm_splay_tree_remove (&usm_allocations, &key);
  usm_splay_tree_insert (&usm_free_space, (usm_splay_tree_node)found);
  usm_coalesce_free_space ();

  /* Release the lock.  */
  __atomic_store_n (&usm_lock, 0, MEMMODEL_RELEASE);
}

#undef ALIGN

/* This allows splay-tree.c to call gomp_fatal in this context.  The splay
   tree code doesn't use the variadic arguments right now.  */
#define gomp_fatal(MSG, ...) GOMP_PLUGIN_fatal (MSG)

/* Include the splay tree code inline, with the prefixes added.  */
#define splay_tree_prefix usm
#define splay_tree_c
#include "splay-tree.h"
