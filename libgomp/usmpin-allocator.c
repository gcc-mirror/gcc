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
   Shared Memory and Pinned Memory.  It allocates memory from a pool allocated
   and configured by the device plugin (for USM), or the OS-specific allocator
   (for pinned).
 
   This implementation keeps the allocated/free chain in a side-table (splay
   tree) to ensure that the allocation routine does not migrate all the USM
   pages back into host memory.  Keeping the meta-data elsewhere is also useful
   for pinned memory, which is typically an extremely limited resource.  */

#include <string.h>
#include "libgomp.h"

/* Use a splay tree to track allocations.  */

typedef struct usmpin_splay_tree_node_s *usmpin_splay_tree_node;
typedef struct usmpin_splay_tree_s *usmpin_splay_tree;
typedef struct usmpin_splay_tree_key_s *usmpin_splay_tree_key;

struct usmpin_splay_tree_key_s {
  void *base;
  size_t size;
};

static inline int
usmpin_splay_compare (usmpin_splay_tree_key x, usmpin_splay_tree_key y)
{
  return (x->base == y->base ? 0
	  : x->base > y->base ? 1
	  : -1);
}

#define splay_tree_prefix usmpin
#include "splay-tree.h"

/* 128-byte granularity means GPU cache-line aligned.  */
#define ALIGN(VAR) (((VAR) + 127) & ~127)

/* The context data prevents the need for global state.  */
struct usmpin_context {
  int lock;
  struct usmpin_splay_tree_s allocations;
  struct usmpin_splay_tree_s free_space;
};

usmpin_ctx_p
usmpin_init_context ()
{
  return calloc (1, sizeof (struct usmpin_context));
}

/* Coalesce contiguous free space into one entry.  This considers the entries
   either side of the root node only, so it should be called each time a new
   entry in inserted into the root.  */

static void
usmpin_coalesce_free_space (usmpin_ctx_p ctx)
{
  usmpin_splay_tree_node prev, next, node = ctx->free_space.root;

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
      usmpin_splay_tree_remove (&ctx->free_space, &next->key);
      free (next);
    }
  if (prev
      && prev->key.base + prev->key.size == node->key.base)
    {
      /* Free chunk precedes.  */
      prev->key.size += node->key.size;
      usmpin_splay_tree_remove (&ctx->free_space, &node->key);
      free (node);
    }
}

/* Add a new memory region into the free chain.  This is how the USM heap is
   initialized and extended.  If the new region is contiguous with an existing
   region then any free space will be coalesced.  */

void
usmpin_register_memory (usmpin_ctx_p ctx, char *base, size_t size)
{
  if (base == NULL || ctx == NULL)
    return;

  while (__atomic_exchange_n (&ctx->lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  usmpin_splay_tree_node node;
  node = malloc (sizeof (struct usmpin_splay_tree_node_s));
  node->key.base = base;
  node->key.size = size;
  node->left = NULL;
  node->right = NULL;
  usmpin_splay_tree_insert (&ctx->free_space, node);
  usmpin_coalesce_free_space (ctx);

  __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);
}

/* This splay_tree_foreach callback selects the first free space large enough
   to hold the allocation needed.  Since the splay_tree walk may start in the
   middle the "first" isn't necessarily the "leftmost" entry.  */

struct usmpin_callback_data {
  size_t size;
  usmpin_splay_tree_node found;
};

static int
usmpin_alloc_callback (usmpin_splay_tree_key key, void *data)
{
  struct usmpin_callback_data *cbd = (struct usmpin_callback_data *)data;

  if (key->size >= cbd->size)
    {
      cbd->found = (usmpin_splay_tree_node)key;
      return 1;
    }

  return 0;
}

/* USM "malloc".  Selects and moves and address range from ctx->free_space to
   ctx->allocations, while leaving any excess in ctx->free_space.  */

void *
usmpin_alloc (usmpin_ctx_p ctx, size_t size)
{
  if (ctx == NULL)
    return NULL;

  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  /* Acquire the lock.  */
  while (__atomic_exchange_n (&ctx->lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  if (!ctx->free_space.root)
    {
      /* No memory registered, or no free space.  */
      __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);
      return NULL;
    }

  /* Find a suitable free block.  */
  struct usmpin_callback_data cbd = {size, NULL};
  usmpin_splay_tree_foreach_lazy (&ctx->free_space, usmpin_alloc_callback,
				  &cbd);
  usmpin_splay_tree_node freenode = cbd.found;

  void *result = NULL;
  if (freenode)
    {
      /* Allocation successful.  */
      result = freenode->key.base;
      usmpin_splay_tree_node allocnode = malloc (sizeof (*allocnode));
      allocnode->key.base = result;
      allocnode->key.size = size;
      allocnode->left = NULL;
      allocnode->right = NULL;
      usmpin_splay_tree_insert (&ctx->allocations, allocnode);

      /* Update the free chain.  */
      size_t stillfree_size = freenode->key.size - size;
      if (stillfree_size > 0)
	{
	  freenode->key.base = freenode->key.base + size;
	  freenode->key.size = stillfree_size;
	}
      else
	{
	  usmpin_splay_tree_remove (&ctx->free_space, &freenode->key);
	  free (freenode);
	}
    }

  /* Release the lock.  */
  __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);

  return result;
}

/* USM "free".  Moves an address range from ctx->allocations to
   ctx->free_space and merges that record with any contiguous free memory.  */

void
usmpin_free (usmpin_ctx_p ctx, void *addr)
{
  if (ctx == NULL)
    return;

  /* Acquire the lock.  */
  while (__atomic_exchange_n (&ctx->lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  /* Convert the memory map to free.  */
  struct usmpin_splay_tree_key_s key = {addr};
  usmpin_splay_tree_key found = usmpin_splay_tree_lookup (&ctx->allocations,
							  &key);
  if (!found)
    GOMP_PLUGIN_fatal ("invalid free");
  usmpin_splay_tree_remove (&ctx->allocations, &key);
  usmpin_splay_tree_insert (&ctx->free_space, (usmpin_splay_tree_node)found);
  usmpin_coalesce_free_space (ctx);

  /* Release the lock.  */
  __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);
}

/* USM "realloc".  Works in-place, if possible; reallocates otherwise.  */

void *
usmpin_realloc (usmpin_ctx_p ctx, void *addr, size_t newsize)
{
  if (ctx == NULL)
    return NULL;

  newsize = ALIGN (newsize);

  /* Acquire the lock.  */
  while (__atomic_exchange_n (&ctx->lock, 1, MEMMODEL_ACQUIRE) == 1)
    ;

  /* Convert the memory map to free.  */
  struct usmpin_splay_tree_key_s key = {addr};
  usmpin_splay_tree_key found = usmpin_splay_tree_lookup (&ctx->allocations,
							  &key);
  if (!found)
    GOMP_PLUGIN_fatal ("invalid realloc");

  if (newsize == found->size)
    ; /* Nothing to do.  */
  else if (newsize < found->size)
    {
      /* We're reducing the allocation size.  */
      usmpin_splay_tree_node newfree = malloc (sizeof (*newfree));
      newfree->key.base = found->base + newsize;
      newfree->key.size = found->size - newsize;
      newfree->left = NULL;
      newfree->right = NULL;
      usmpin_splay_tree_insert (&ctx->free_space, newfree);
      usmpin_coalesce_free_space (ctx);
    }
  else
    {
      /* We're extending the allocation.  */
      struct usmpin_splay_tree_key_s freekey = {addr + found->size};
      usmpin_splay_tree_key foundfree;
      foundfree = usmpin_splay_tree_lookup (&ctx->free_space, &freekey);
      if (foundfree && foundfree->size >= newsize - found->size)
	{
	  /* Allocation can be expanded in place.  */
	  foundfree->base += found->size;
	  foundfree->size -= newsize - found->size;
	  found->size = newsize;

	  if (foundfree->size == 0)
	    usmpin_splay_tree_remove (&ctx->free_space, &freekey);
	}
      else
	{
	  /* Allocation must be relocated.
	     Release the lock and use alloc/free.  */
	  __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);

	  void *newaddr = usmpin_alloc (ctx, newsize);
	  if (!newaddr)
	    return NULL;

	  memcpy (newaddr, addr, found->size);
	  usmpin_free (ctx, addr);
	  return newaddr;
	}
    }

  /* Release the lock.  */
  __atomic_store_n (&ctx->lock, 0, MEMMODEL_RELEASE);
  return addr;
}

/* Include the splay tree code inline, with the prefixes added.  */
#define splay_tree_prefix usmpin
#define splay_tree_c
#include "splay-tree.h"
