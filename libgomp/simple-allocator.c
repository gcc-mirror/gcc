/* Copyright (C) 2025 Free Software Foundation, Inc.

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

/* This is a simple "malloc" implementation intended for use with device
   Managed Memory and Pinned Memory.  It allocates memory from a pool allocated
   and configured by the device plugin, or the OS-specific allocator
   (for pinned).
 
   Unlike the "basic" allocator, this implementation keeps the allocated/free
   chain in a side-table (splay tree) to ensure that the allocation routine
   does not have the side-effect of migrating all the managed memory pages back
   into host memory.  Keeping the meta-data elsewhere is also useful for pinned
   memory, which is typically an extremely limited resource.  */

#include <string.h>
#include "libgomp.h"

/* Use a splay tree to track allocations.  */

typedef struct simple_alloc_splay_tree_node_s *simple_alloc_splay_tree_node;
typedef struct simple_alloc_splay_tree_s *simple_alloc_splay_tree;
typedef struct simple_alloc_splay_tree_key_s *simple_alloc_splay_tree_key;

struct simple_alloc_splay_tree_key_s {
  void *base;
  size_t size;
};

static inline int
simple_alloc_splay_compare (simple_alloc_splay_tree_key x,
			    simple_alloc_splay_tree_key y)
{
  return (x->base == y->base ? 0
	  : x->base > y->base ? 1
	  : -1);
}

#define splay_tree_prefix simple_alloc
#include "splay-tree.h"

/* 128-byte granularity means GPU cache-line aligned.  */
#define ALIGN(VAR) (((VAR) + 127) & ~127)

/* The context data prevents the need for global state.  */
struct gomp_simple_alloc_context {
  struct simple_alloc_splay_tree_s allocations;
  struct simple_alloc_splay_tree_s free_space;
  gomp_mutex_t lock;
};

gomp_simple_alloc_ctx_p
gomp_simple_alloc_init_context ()
{
  return calloc (1, sizeof (struct gomp_simple_alloc_context));
}

/* Coalesce contiguous free space into one entry.  This considers the entries
   either side of the root node only, so it should be called each time a new
   entry in inserted into the root.  */

static void
simple_alloc_coalesce_free_space (gomp_simple_alloc_ctx_p ctx)
{
  simple_alloc_splay_tree_node prev, next, node = ctx->free_space.root;

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
      simple_alloc_splay_tree_remove (&ctx->free_space, &next->key);
      free (next);
    }
  if (prev
      && prev->key.base + prev->key.size == node->key.base)
    {
      /* Free chunk precedes.  */
      prev->key.size += node->key.size;
      simple_alloc_splay_tree_remove (&ctx->free_space, &node->key);
      free (node);
    }
}

/* Add a new memory region into the free chain.  This is how our heap is
   initialized and extended (using memory acquired by an external caller).  If
   the new region is contiguous with an existing region then any free space
   will be coalesced.  */

void
gomp_simple_alloc_register_memory (gomp_simple_alloc_ctx_p ctx, char *base,
				   size_t size)
{
  if (base == NULL || ctx == NULL)
    return;

  gomp_mutex_lock (&ctx->lock);

  simple_alloc_splay_tree_node node;
  node = malloc (sizeof (struct simple_alloc_splay_tree_node_s));
  node->key.base = base;
  node->key.size = size;
  node->left = NULL;
  node->right = NULL;
  simple_alloc_splay_tree_insert (&ctx->free_space, node);
  simple_alloc_coalesce_free_space (ctx);

  gomp_mutex_unlock (&ctx->lock);
}

/* This splay_tree_foreach callback selects the first free space large enough
   to hold the allocation needed.  Since the splay_tree walk may start in the
   middle the "first" isn't necessarily the "leftmost" entry.  */

struct simple_alloc_callback_data {
  size_t size;
  simple_alloc_splay_tree_node found;
};

static int
simple_alloc_callback (simple_alloc_splay_tree_key key, void *data)
{
  struct simple_alloc_callback_data *cbd
    = (struct simple_alloc_callback_data *)data;

  if (key->size >= cbd->size)
    {
      cbd->found = (simple_alloc_splay_tree_node)key;
      return 1;
    }

  return 0;
}

/* Simple "malloc".  Selects and moves and address range from ctx->free_space to
   ctx->allocations, while leaving any excess in ctx->free_space.  */

void *
gomp_simple_alloc (gomp_simple_alloc_ctx_p ctx, size_t size)
{
  if (ctx == NULL)
    return NULL;

  /* Memory is allocated in N-byte granularity.  */
  size = ALIGN (size);

  gomp_mutex_lock (&ctx->lock);

  if (!ctx->free_space.root)
    {
      /* No memory registered, or no free space.  */
      gomp_mutex_unlock (&ctx->lock);
      return NULL;
    }

  /* Find a suitable free block.  */
  struct simple_alloc_callback_data cbd = {size, NULL};
  simple_alloc_splay_tree_foreach_lazy (&ctx->free_space,
					simple_alloc_callback, &cbd);
  simple_alloc_splay_tree_node freenode = cbd.found;

  void *result = NULL;
  if (freenode)
    {
      /* Allocation successful.  */
      result = freenode->key.base;
      simple_alloc_splay_tree_node allocnode = malloc (sizeof (*allocnode));
      allocnode->key.base = result;
      allocnode->key.size = size;
      allocnode->left = NULL;
      allocnode->right = NULL;
      simple_alloc_splay_tree_insert (&ctx->allocations, allocnode);

      /* Update the free chain.  */
      size_t stillfree_size = freenode->key.size - size;
      if (stillfree_size > 0)
	{
	  freenode->key.base = freenode->key.base + size;
	  freenode->key.size = stillfree_size;
	}
      else
	{
	  simple_alloc_splay_tree_remove (&ctx->free_space, &freenode->key);
	  free (freenode);
	}
    }

  gomp_mutex_unlock (&ctx->lock);

  return result;
}

/* Simple "free".  Moves an address range from ctx->allocations to
   ctx->free_space and merges that record with any contiguous free memory.  */

void
gomp_simple_free (gomp_simple_alloc_ctx_p ctx, void *addr)
{
  if (ctx == NULL)
    return;

  gomp_mutex_lock (&ctx->lock);

  /* Convert the memory map to free.  */
  struct simple_alloc_splay_tree_key_s key = {addr};
  simple_alloc_splay_tree_key found
    = simple_alloc_splay_tree_lookup (&ctx->allocations, &key);
  if (!found)
    GOMP_PLUGIN_fatal ("invalid free");
  simple_alloc_splay_tree_remove (&ctx->allocations, &key);
  simple_alloc_splay_tree_insert (&ctx->free_space,
				  (simple_alloc_splay_tree_node)found);
  simple_alloc_coalesce_free_space (ctx);

  gomp_mutex_unlock (&ctx->lock);
}

/* Simple "realloc".  Works in-place, if possible; reallocates otherwise.  */

void *
gomp_simple_realloc (gomp_simple_alloc_ctx_p ctx, void *addr, size_t newsize)
{
  if (ctx == NULL)
    return NULL;

  newsize = ALIGN (newsize);

  gomp_mutex_lock (&ctx->lock);

  /* Convert the memory map to free.  */
  struct simple_alloc_splay_tree_key_s key = {addr};
  simple_alloc_splay_tree_key found
    = simple_alloc_splay_tree_lookup (&ctx->allocations, &key);
  if (!found)
    GOMP_PLUGIN_fatal ("invalid realloc");

  if (newsize == found->size)
    ; /* Nothing to do.  */
  else if (newsize < found->size)
    {
      /* We're reducing the allocation size.  */
      simple_alloc_splay_tree_node newfree = malloc (sizeof (*newfree));
      newfree->key.base = found->base + newsize;
      newfree->key.size = found->size - newsize;
      newfree->left = NULL;
      newfree->right = NULL;
      simple_alloc_splay_tree_insert (&ctx->free_space, newfree);
      simple_alloc_coalesce_free_space (ctx);
    }
  else
    {
      /* We're extending the allocation.  */
      struct simple_alloc_splay_tree_key_s freekey = {addr + found->size};
      simple_alloc_splay_tree_key foundfree;
      foundfree = simple_alloc_splay_tree_lookup (&ctx->free_space, &freekey);
      if (foundfree && foundfree->size >= newsize - found->size)
	{
	  /* Allocation can be expanded in place.  */
	  foundfree->base += found->size;
	  foundfree->size -= newsize - found->size;
	  found->size = newsize;

	  if (foundfree->size == 0)
	    simple_alloc_splay_tree_remove (&ctx->free_space, &freekey);
	}
      else
	{
	  /* Allocation must be relocated.
	     Release the lock and use alloc/free.  */
	  gomp_mutex_unlock (&ctx->lock);

	  void *newaddr = gomp_simple_alloc (ctx, newsize);
	  if (!newaddr)
	    return NULL;

	  memcpy (newaddr, addr, found->size);
	  gomp_simple_free (ctx, addr);
	  return newaddr;
	}
    }

  gomp_mutex_unlock (&ctx->lock);
  return addr;
}

/* Include the splay tree code inline, with the prefixes added.  */
#define splay_tree_prefix simple_alloc
#define splay_tree_c
#define gomp_fatal GOMP_PLUGIN_fatal  /* So it links into a plugin.  */
#include "splay-tree.h"
