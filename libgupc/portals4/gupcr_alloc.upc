/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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


/**
 * @file gupcr_alloc.upc
 * GUPC Portals4 UPC dynamic shared memory allocation.
 *
 * Implement UPC's dynamic memory allocation routines.
 * The implementation is written in UPC, because
 * it needs to run above the runtime library's memory mapping
 * facility.  Internal runtime spin locks are used rather than
 * the UPC language-defined locks, because those locks
 * depend upon dynamic memory management, and we need to
 * break the circular dependency.
 *
 * @addtogroup ALLOC GUPCR Shared Memory Allocator Functions
 * @{
 */

#include <upc.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_barrier.h"
#include "gupcr_lock.h"

struct upc_heap_list_struct;
typedef shared struct upc_heap_list_struct *upc_heap_list_p;
typedef struct upc_heap_list_struct
{
  upc_heap_list_p next;
  upc_heap_list_p prev;
} upc_heap_list_t;
typedef upc_heap_list_t upc_heap_pool_t[GUPCR_HEAP_NUM_POOLS];
typedef struct upc_heap_struct
{
  shared void *base;
  upc_lock_t *lock;
  size_t size;
  int is_global;
  size_t pool_avail;
  upc_heap_pool_t pool;
} upc_heap_t;
typedef shared upc_heap_t *upc_heap_p;

typedef struct upc_heap_node_struct
{
  upc_heap_list_t link;		/* Must be first.  */
  size_t size;
  int alloc_tag;
  int is_global;
} upc_heap_node_t;
typedef shared upc_heap_node_t *upc_heap_node_p;

static shared void *gupcr_heap_region_base;
static shared void *gupcr_heap_region_top;
static size_t gupcr_heap_region_size;

static shared upc_heap_t gupcr_global_heap_info;
static shared upc_heap_t gupcr_local_heap_info[THREADS];
static strict shared size_t gupcr_heap_global_hi_water_mark;
static strict shared size_t gupcr_heap_local_low_water_mark;
static upc_heap_p gupcr_global_heap;
static upc_heap_p gupcr_local_heap;

/** Return the smallest power of 2 that is >= 'v',
    scaled so that gupcr_log2 of the minimum allocation size is 0.  */
static inline unsigned int
gupcr_plog2 (unsigned long long v)
{
  return gupcr_log2 (GUPCR_MAX (v, GUPCR_HEAP_ALLOC_MIN)) -
    GUPCR_HEAP_ALLOC_MIN_BITS;
}

/** Return TRUE if 'list' is empty.  */
static inline int
gupcr_heap_is_empty_list (upc_heap_list_p list)
{
  gupcr_assert (list != NULL);
  return list->next == list;
}

/** Insert 'node' after 'here' in the double linked free list.  */
static inline void
gupcr_heap_list_insert (upc_heap_list_p here, upc_heap_list_p node)
{
  upc_heap_list_p next;
  gupcr_assert (here != NULL);
  gupcr_assert (node != NULL);
  next = here->next;
  gupcr_assert (next != NULL);
  node->next = next;
  node->prev = here;
  next->prev = node;
  here->next = node;
}

/** Remove 'node' from its position in doubly-linked free list.  */
static inline void
gupcr_heap_list_remove (upc_heap_list_p node)
{
  upc_heap_list_p next, prev;
  gupcr_assert (node != NULL);
  prev = node->prev;
  gupcr_assert (prev != NULL);
  next = node->next;
  gupcr_assert (next != NULL);
  prev->next = next;
  next->prev = prev;
  node->next = NULL;
  node->prev = NULL;
}

/** Pop a node from the front of the free list
    rooted at the 'p'-th pool in 'heap'.  */
static inline upc_heap_node_p
gupcr_heap_list_pop (upc_heap_p heap, unsigned int p)
{
  upc_heap_node_p node = NULL;
  upc_heap_list_p list;
  gupcr_assert (heap != NULL);
  gupcr_assert (p < GUPCR_HEAP_NUM_POOLS);
  list = (upc_heap_list_p) &heap->pool[p];
  if (!gupcr_heap_is_empty_list (list))
    {
      upc_heap_list_p first;
      first = list->next;
      gupcr_assert (first != NULL);
      gupcr_heap_list_remove (first);
      node = (upc_heap_node_p) first;
      if (gupcr_heap_is_empty_list (list))
	heap->pool_avail = gupcr_clear_bit (heap->pool_avail, p);
    }
  return node;
}

/** Push 'node' onto the front of the free list
    rooted at the 'p'-th pool in 'heap'.  */
static inline void
gupcr_heap_list_push (upc_heap_p heap, unsigned int p, upc_heap_node_p node)
{
  upc_heap_list_p list;
  gupcr_assert (heap != NULL);
  gupcr_assert (node != NULL);
  gupcr_assert (p < GUPCR_HEAP_NUM_POOLS);
  list = (upc_heap_list_p) &heap->pool[p];
  if (gupcr_heap_is_empty_list (list))
    heap->pool_avail = gupcr_set_bit (heap->pool_avail, p);
  gupcr_heap_list_insert (list, (upc_heap_list_p) node);
}

/**
 * Split 'node' into two nodes each of half the size.
 *
 * Push one of the half-sized nodes back onto an appropriate free list.
 * Return the other half-size node.  Before calling this routine,
 * 'node' must not be on any free list.
 */
static inline upc_heap_node_p
gupcr_heap_list_split (upc_heap_p heap, upc_heap_node_p node)
{
  size_t node_size;
  size_t half_size;
  unsigned int is_global;
  upc_heap_node_p free_half;
  unsigned int p;
  gupcr_assert (heap != NULL);
  gupcr_assert (node != NULL);
  is_global = heap->is_global;
  node_size = node->size;
  half_size = ((size_t) 1 << (gupcr_log2 (node_size) - 1));
  p = gupcr_plog2 (half_size);
  if (is_global)
    free_half = gupcr_pts_add_offset (node, half_size);
  else
    {
      free_half = node;
      node = gupcr_pts_add_offset (free_half, half_size);
    }
  upc_memset (free_half, '\0', GUPCR_HEAP_ALLOC_OVERHEAD);
  free_half->size = half_size;
  free_half->is_global = is_global;
  gupcr_heap_list_push (heap, p, free_half);
  upc_memset (node, '\0', GUPCR_HEAP_ALLOC_OVERHEAD);
  node->size = half_size;
  node->is_global = is_global;
  return node;
}

/**
 * Return the buddy of 'node'.
 *
 * The buddy is calculated at binary level 'p' by exclusive or-ing
 * the p'th bit of the offset of 'node' within the heap.
 * If there is no buddy for this block, return NULL.
 */
static inline upc_heap_node_p
gupcr_heap_get_buddy (upc_heap_p heap, upc_heap_node_p node)
{
  shared void *heap_base;
  size_t heap_size;
  ptrdiff_t heap_offset, buddy_offset, max_buddy_offset;
  unsigned int p;
  upc_heap_node_p buddy = NULL;
  gupcr_assert (heap != NULL);
  gupcr_assert (node != NULL);
  heap_base = heap->base;
  heap_size = heap->size;
  heap_offset = gupcr_pts_diff (node, heap_base);
  gupcr_assert (heap_offset >= 0);
  p = gupcr_log2 (node->size);
  buddy_offset = heap_offset ^ ((ptrdiff_t) 1 << p);
  max_buddy_offset = (ptrdiff_t) heap_size - GUPCR_HEAP_ALLOC_MIN;
  if (buddy_offset <= max_buddy_offset)
    buddy = gupcr_pts_add_offset (heap_base, buddy_offset);
  return buddy;
}

/**
 * Attempt to join the node pointed to by 'node_ref'
 * to its buddy in 'heap' of log2 size 'p'.
 *
 * Return TRUE if successful.  If the buddy node
 * is the 'left' buddy, update the node pointed
 * to by 'node_ref' to point to the buddy.
 */
static inline unsigned int
gupcr_heap_list_join (upc_heap_p heap,
		      unsigned int p, upc_heap_node_p *node_ref)
{
  unsigned int joined = 0;
  upc_heap_node_p buddy, node;
  gupcr_assert (heap != NULL);
  gupcr_assert (node_ref);
  gupcr_assert (p < GUPCR_HEAP_NUM_POOLS);
  node = *node_ref;
  gupcr_assert (node != NULL);
  buddy = gupcr_heap_get_buddy (heap, node);
  /* The node can be joined with its buddy if:
     1. The buddy is free.
     2. The buddy has the same power-of-2 size.  */
  if (buddy && !buddy->alloc_tag)
    {
      unsigned int p_buddy;
      gupcr_assert (buddy->size > 0);
      p_buddy = gupcr_plog2 (buddy->size);
      if (p == p_buddy)
	{
	  unsigned int p_above = p + 1;
	  upc_heap_list_p list;
	  joined = 1;
	  gupcr_heap_list_remove ((upc_heap_list_p) buddy);
	  list = (upc_heap_list_p) &heap->pool[p];
	  if (gupcr_heap_is_empty_list (list))
	    heap->pool_avail = gupcr_clear_bit (heap->pool_avail, p);
	  if (gupcr_pts_diff (buddy, node) < 0)
	    {
	      node = buddy;
	      *node_ref = node;
	    }
	  node->alloc_tag = 0;
	  node->size = ((size_t) 1 << (p_above + GUPCR_HEAP_ALLOC_MIN_BITS));
	}
    }
  return joined;
}

/**
 * Initialize the data structure used to manage
 * operations on 'heap'.
 *
 * 'is_global' is TRUE if the heap is a global heap.
 *
 * For global  heaps, 'base' points to the bottom of the heap
 * storage area.  For local heaps, 'base' initially points
 * to the top of the heap storage area and then grows downward.
 */
static inline void
gupcr_heap_init_info (upc_heap_p heap,
		      unsigned int is_global, shared void *base)
{
  unsigned int p;
  shared [] upc_heap_list_t *pool;
  gupcr_assert (heap != NULL);
  upc_memset (heap, '\0', sizeof (upc_heap_t));
  gupcr_assert (base != NULL);
  heap->base = base;
  heap->is_global = is_global;
  if (is_global)
    heap->lock = gupcr_global_heap_lock;
  else
    heap->lock = gupcr_local_heap_lock;
  for (p = 0, pool = &heap->pool[0]; p < GUPCR_HEAP_NUM_POOLS; ++p, ++pool)
    {
      pool->next = (upc_heap_list_p) pool;
      pool->prev = (upc_heap_list_p) pool;
    }
  heap->pool_avail = 0;
}

/**
 * Initialize the global and local heap data structures.
 *
 * 'heap_region_base' is the shared address where the heap should begin,
 * and 'heap_region_size' is the maximum number of bytes available
 * for dynamic shared memory allocation.
 */
void
gupcr_alloc_init (shared void *heap_region_base, size_t heap_region_size)
{
  shared void *local_heap_base;
  gupcr_assert (upc_threadof (heap_region_base) == (size_t) MYTHREAD);
  gupcr_assert (gupcr_is_pow_2 (GUPCR_HEAP_ALLOC_MIN));
  gupcr_assert ((GUPCR_HEAP_ALLOC_OVERHEAD % 16) == 0);
  gupcr_assert (GUPCR_HEAP_ALLOC_OVERHEAD >= sizeof (upc_heap_node_t));
  gupcr_heap_region_base = heap_region_base;
  gupcr_heap_region_size = heap_region_size;
  gupcr_heap_region_top =
    gupcr_pts_add_offset (heap_region_base, heap_region_size);
  gupcr_global_heap = &gupcr_global_heap_info;
  gupcr_local_heap = &gupcr_local_heap_info[MYTHREAD];
  if (!MYTHREAD)
    {
      gupcr_heap_global_hi_water_mark = 0;
      gupcr_heap_local_low_water_mark = heap_region_size;
      gupcr_heap_init_info (&gupcr_global_heap_info, 1, heap_region_base);
    }
  /* The local heap base is initially the top of the UPC heap region,  */
  local_heap_base = gupcr_heap_region_top;
  gupcr_heap_init_info (gupcr_local_heap, 0, local_heap_base);
}

/**
 * Allocate 'size' bytes from the heap memory region.
 *
 * Global allocations raise the high water mark.
 * Local allocations potentially decrease the low water mark.
 * Space is available as long as the high water mark
 * does not cross above the low water mark.
 *
 * If successful, return a pointer to the newly allocated space.
 * Return NULL if there is not enough space.
 *
 * The 'size' argument is constrained to be an exact power of 2.
 *
 */
static shared void *
gupcr_heap_region_alloc (upc_heap_p heap, size_t size)
{
  shared void *mem = NULL;
  unsigned int is_global;
  size_t heap_size, new_heap_size;
  shared void *heap_base;
  unsigned int have_enough_space;
  gupcr_assert (heap != NULL);
  gupcr_assert (size > 0);
  gupcr_assert (gupcr_is_pow_2 (size));
  is_global = heap->is_global;
  heap_size = heap->size;
  heap_base = heap->base;
  new_heap_size = heap_size + size;
  have_enough_space = 0;
  upc_lock (gupcr_heap_region_lock);
  if (is_global)
    {
      size_t new_hi_water_mark;
      new_hi_water_mark = new_heap_size;
      if (new_hi_water_mark <= gupcr_heap_local_low_water_mark)
	{
	  gupcr_heap_global_hi_water_mark = new_hi_water_mark;
	  have_enough_space = 1;
	}
    }
  else
    {
      if (new_heap_size <= gupcr_heap_region_size)
	{
	  size_t new_low_water_mark;
	  new_low_water_mark = gupcr_heap_region_size - new_heap_size;
	  if (new_low_water_mark >= gupcr_heap_global_hi_water_mark)
	    {
	      if (new_low_water_mark < gupcr_heap_local_low_water_mark)
		gupcr_heap_local_low_water_mark = new_low_water_mark;
	      have_enough_space = 1;
	    }
	}
    }
  upc_unlock (gupcr_heap_region_lock);
  if (have_enough_space)
    {
      ptrdiff_t heap_size_offset;
      if (is_global)
	{
	  heap_size_offset = (ptrdiff_t) heap_size;
	  mem = gupcr_pts_add_offset (heap_base, heap_size_offset);
	}
      else
	{
	  heap_size_offset = -((ptrdiff_t) size);
	  heap_base = gupcr_pts_add_offset (heap_base, heap_size_offset);
	  heap->base = heap_base;
	  mem = heap_base;
	}
      heap->size = new_heap_size;
    }
  return mem;
}

/**
 * Repetitively double the size of 'heap' until a free block
 * of at least 'size' bytes (rounded up to the next power of 2)
 * is created.
 */
static void
gupcr_heap_extend (upc_heap_p heap, size_t size)
{
  size_t heap_size;
  size_t extend_size;
  unsigned int is_global;
  unsigned int p;
  size_t free_block_size;
  gupcr_assert (heap != NULL);
  gupcr_assert (size > 0);
  heap_size = heap->size;
  is_global = heap->is_global;
  extend_size = ((size_t) 1 << gupcr_log2 (size));
  do
    {
      upc_heap_node_p free_block;
      free_block_size = heap_size ? heap_size : extend_size;
      free_block = gupcr_heap_region_alloc (heap, free_block_size);
      if (free_block == NULL)
	return;
      upc_memset (free_block, '\0', GUPCR_HEAP_ALLOC_OVERHEAD);
      free_block->size = free_block_size;
      free_block->is_global = is_global;
      p = gupcr_plog2 (free_block_size);
      gupcr_heap_list_push (heap, p, free_block);
      heap_size += free_block_size;
      heap->size = heap_size;
    }
  while (free_block_size < extend_size);
}

/**
 * Allocate a block of 'size' bytes from 'heap'.
 */
static shared void *
gupcr_heap_alloc (upc_heap_p heap, size_t size)
{
  shared void *mem = NULL;
  const size_t alloc_size = GUPCR_MAX (size + GUPCR_HEAP_ALLOC_OVERHEAD,
				       GUPCR_HEAP_ALLOC_MIN);
  const unsigned int pool_fit = gupcr_plog2 (alloc_size);
  unsigned long long int pool_avail;
  unsigned int p;
  upc_heap_node_p alloc = NULL;
  gupcr_assert (heap != NULL);
  gupcr_assert (size > 0);
  upc_lock (heap->lock);
  pool_avail = heap->pool_avail << pool_fit;
  if (!pool_avail)
    {
      gupcr_heap_extend (heap, alloc_size);
      pool_avail = heap->pool_avail << pool_fit;
    }
  if (pool_avail)
    {
      p = pool_fit + gupcr_find_first_one (pool_avail);
      for (alloc = gupcr_heap_list_pop (heap, p); p > pool_fit; --p)
	alloc = gupcr_heap_list_split (heap, alloc);
      alloc->alloc_tag = GUPCR_HEAP_ALLOC_TAG;
    }
  if (alloc)
    mem = gupcr_pts_add_offset (alloc, GUPCR_HEAP_ALLOC_OVERHEAD);
  upc_unlock (heap->lock);
  return mem;
}

/**
 * Return the block given by 'node' into 'heap'.
 */
static void
gupcr_heap_free (upc_heap_p heap, upc_heap_node_p node)
{
  unsigned int p;
  upc_heap_node_p free_node;
  gupcr_assert (heap != NULL);
  gupcr_assert (node != NULL);
  upc_lock (heap->lock);
  for (p = gupcr_plog2 (node->size), free_node = node;
       gupcr_heap_list_join (heap, p, &free_node); ++p) /* loop */ ;
  free_node->alloc_tag = 0;
  gupcr_heap_list_push (heap, p, free_node);
  upc_unlock (heap->lock);
}

shared void *
upc_global_alloc (size_t nblocks, size_t nbytes)
{
  size_t request_size = GUPCR_ROUND (nblocks, THREADS) * nbytes;
  size_t alloc_size = request_size / THREADS;
  shared void *mem = NULL;
  gupcr_trace (FC_ALLOC, "ALLOC GLOBAL_ALLOC ENTER");
  if (alloc_size > 0)
    mem = gupcr_heap_alloc (gupcr_global_heap, alloc_size);
  gupcr_trace (FC_ALLOC, "ALLOC GLOBAL_ALLOC EXIT %u:0x%lx %lu",
	       (unsigned) upc_threadof (mem),
	       (long unsigned) upc_addrfield (mem),
	       (long unsigned) nbytes);
  return mem;
}

shared void *
upc_all_alloc (size_t nblocks, size_t nbytes)
{
  size_t request_size = GUPCR_ROUND (nblocks, THREADS) * nbytes;
  size_t alloc_size = request_size / THREADS;
  shared void *mem = NULL;
  gupcr_trace (FC_ALLOC, "ALLOC ALL_ALLOC ENTER");
  if (alloc_size > 0)
    {
      if (MYTHREAD == 0)
	{
	  mem = gupcr_heap_alloc (gupcr_global_heap, alloc_size);
	  gupcr_bcast_send (&mem, sizeof (mem));
	}
      else
	gupcr_bcast_recv (&mem, sizeof (mem));
    }
  gupcr_trace (FC_ALLOC, "ALLOC ALL_ALLOC EXIT %u:0x%lx %lu",
	       (unsigned) upc_threadof (mem),
	       (long unsigned) upc_addrfield (mem),
	       (long unsigned) nbytes);
  return mem;
}

shared void *
upc_alloc (size_t nbytes)
{
  shared void *mem = NULL;
  gupcr_trace (FC_ALLOC, "ALLOC ALLOC ENTER");
  if (nbytes)
    mem = gupcr_heap_alloc (gupcr_local_heap, nbytes);
  gupcr_trace (FC_ALLOC, "ALLOC ALLOC EXIT %u:0x%lx %lu",
	       (unsigned) upc_threadof (mem),
	       (long unsigned) upc_addrfield (mem),
	       (long unsigned) nbytes);
  return mem;
}

void
upc_all_free (shared void *ptr)
{
  if (ptr)
    {
      const int thread = (int) upc_threadof (ptr);
      upc_barrier - 1;
      /* Check for errors only on thread 0.  */
      if ((MYTHREAD == 0) && (thread >= THREADS))
	gupcr_error ("upc_all_free() called with invalid shared pointer");
      if (thread == MYTHREAD)
	upc_free (ptr);
    }
}

void
upc_free (shared void *ptr)
{
  gupcr_trace (FC_ALLOC, "ALLOC FREE ENTER %u:0x%lx",
	       (unsigned) upc_threadof (ptr),
	       (long unsigned) upc_addrfield (ptr));
  if (ptr)
    {
      const size_t offset __attribute__ ((unused)) = upc_addrfield (ptr);
      const int thread = (int) upc_threadof (ptr);
      const size_t phase = upc_phaseof (ptr);
      upc_heap_p heap;
      upc_heap_node_p node;
      unsigned int is_global;
      if (phase || thread >= THREADS)
	gupcr_error ("upc_free() called with invalid shared pointer");
      node = gupcr_pts_add_offset (ptr, -GUPCR_HEAP_ALLOC_OVERHEAD);
      is_global = node->is_global;
      if (is_global && thread)
	gupcr_error ("upc_free() called with invalid shared pointer");
      if (node->alloc_tag != GUPCR_HEAP_ALLOC_TAG)
	gupcr_error ("upc_free() called with pointer to unallocated space");
      if (is_global)
	heap = gupcr_global_heap;
      else
	heap = &gupcr_local_heap_info[thread];
      gupcr_heap_free (heap, node);
    }
  gupcr_trace (FC_ALLOC, "ALLOC FREE EXIT");
}

/** @} */
