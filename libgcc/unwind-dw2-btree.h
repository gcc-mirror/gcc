/* Lock-free btree for manually registered unwind frames.  */
/* Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Thomas Neumann

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_UNWIND_DW2_BTREE_H
#define GCC_UNWIND_DW2_BTREE_H

#include <stdbool.h>

// Common logic for version locks.
struct version_lock
{
  // The lock itself. The lowest bit indicates an exclusive lock,
  // the second bit indicates waiting threads. All other bits are
  // used as counter to recognize changes.
  // Overflows are okay here, we must only prevent overflow to the
  // same value within one lock_optimistic/validate
  // range. Even on 32 bit platforms that would require 1 billion
  // frame registrations within the time span of a few assembler
  // instructions.
  uintptr_type version_lock;
};

#ifdef __GTHREAD_HAS_COND
// We should never get contention within the tree as it rarely changes.
// But if we ever do get contention we use these for waiting.
static __gthread_mutex_t version_lock_mutex = __GTHREAD_MUTEX_INIT;
static __gthread_cond_t version_lock_cond = __GTHREAD_COND_INIT;
#endif

// Initialize in locked state.
static inline void
version_lock_initialize_locked_exclusive (struct version_lock *vl)
{
  vl->version_lock = 1;
}

// Try to lock the node exclusive.
static inline bool
version_lock_try_lock_exclusive (struct version_lock *vl)
{
  uintptr_type state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  if (state & 1)
    return false;
  return __atomic_compare_exchange_n (&(vl->version_lock), &state, state | 1,
				      false, __ATOMIC_SEQ_CST,
				      __ATOMIC_SEQ_CST);
}

// Lock the node exclusive, blocking as needed.
static void
version_lock_lock_exclusive (struct version_lock *vl)
{
#ifndef __GTHREAD_HAS_COND
restart:
#endif

  // We should virtually never get contention here, as frame
  // changes are rare.
  uintptr_type state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  if (!(state & 1))
    {
      if (__atomic_compare_exchange_n (&(vl->version_lock), &state, state | 1,
				       false, __ATOMIC_SEQ_CST,
				       __ATOMIC_SEQ_CST))
	return;
    }

    // We did get contention, wait properly.
#ifdef __GTHREAD_HAS_COND
  __gthread_mutex_lock (&version_lock_mutex);
  state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  while (true)
    {
      // Check if the lock is still held.
      if (!(state & 1))
	{
	  if (__atomic_compare_exchange_n (&(vl->version_lock), &state,
					   state | 1, false, __ATOMIC_SEQ_CST,
					   __ATOMIC_SEQ_CST))
	    {
	      __gthread_mutex_unlock (&version_lock_mutex);
	      return;
	    }
	  else
	    {
	      continue;
	    }
	}

      // Register waiting thread.
      if (!(state & 2))
	{
	  if (!__atomic_compare_exchange_n (&(vl->version_lock), &state,
					    state | 2, false, __ATOMIC_SEQ_CST,
					    __ATOMIC_SEQ_CST))
	    continue;
	}

      // And sleep.
      __gthread_cond_wait (&version_lock_cond, &version_lock_mutex);
      state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
    }
#else
  // Spin if we do not have condition variables available.
  // We expect no contention here, spinning should be okay.
  goto restart;
#endif
}

// Release a locked node and increase the version lock.
static void
version_lock_unlock_exclusive (struct version_lock *vl)
{
  // increase version, reset exclusive lock bits
  uintptr_type state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  uintptr_type ns = (state + 4) & (~((uintptr_type) 3));
  state = __atomic_exchange_n (&(vl->version_lock), ns, __ATOMIC_SEQ_CST);

#ifdef __GTHREAD_HAS_COND
  if (state & 2)
    {
      // Wake up waiting threads. This should be extremely rare.
      __gthread_mutex_lock (&version_lock_mutex);
      __gthread_cond_broadcast (&version_lock_cond);
      __gthread_mutex_unlock (&version_lock_mutex);
    }
#endif
}

// Acquire an optimistic "lock". Note that this does not lock at all, it
// only allows for validation later.
static inline bool
version_lock_lock_optimistic (const struct version_lock *vl, uintptr_type *lock)
{
  uintptr_type state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  *lock = state;

  // Acquiring the lock fails when there is currently an exclusive lock.
  return !(state & 1);
}

// Validate a previously acquired "lock".
static inline bool
version_lock_validate (const struct version_lock *vl, uintptr_type lock)
{
  // Prevent the reordering of non-atomic loads behind the atomic load.
  // Hans Boehm, Can Seqlocks Get Along with Programming Language Memory
  // Models?, Section 4.
  __atomic_thread_fence (__ATOMIC_ACQUIRE);

  // Check that the node is still in the same state.
  uintptr_type state = __atomic_load_n (&(vl->version_lock), __ATOMIC_SEQ_CST);
  return (state == lock);
}

// The largest possible separator value.
static const uintptr_type max_separator = ~((uintptr_type) (0));

struct btree_node;

// Inner entry. The child tree contains all entries <= separator.
struct inner_entry
{
  uintptr_type separator;
  struct btree_node *child;
};

// Leaf entry. Stores an object entry.
struct leaf_entry
{
  uintptr_type base, size;
  struct object *ob;
};

// Node types.
enum node_type
{
  btree_node_inner,
  btree_node_leaf,
  btree_node_free
};

// Node sizes. Chosen such that the result size is roughly 256 bytes.
#define max_fanout_inner 15
#define max_fanout_leaf 10

// A btree node.
struct btree_node
{
  // The version lock used for optimistic lock coupling.
  struct version_lock version_lock;
  // The number of entries.
  unsigned entry_count;
  // The type.
  enum node_type type;
  // The payload.
  union
  {
    // The inner nodes have fence keys, i.e., the right-most entry includes a
    // separator.
    struct inner_entry children[max_fanout_inner];
    struct leaf_entry entries[max_fanout_leaf];
  } content;
};

// Is an inner node?
static inline bool
btree_node_is_inner (const struct btree_node *n)
{
  return n->type == btree_node_inner;
}

// Is a leaf node?
static inline bool
btree_node_is_leaf (const struct btree_node *n)
{
  return n->type == btree_node_leaf;
}

// Should the node be merged?
static inline bool
btree_node_needs_merge (const struct btree_node *n)
{
  return n->entry_count < (btree_node_is_inner (n) ? (max_fanout_inner / 2)
						   : (max_fanout_leaf / 2));
}

// Get the fence key for inner nodes.
static inline uintptr_type
btree_node_get_fence_key (const struct btree_node *n)
{
  // For inner nodes we just return our right-most entry.
  return n->content.children[n->entry_count - 1].separator;
}

// Find the position for a slot in an inner node.
static unsigned
btree_node_find_inner_slot (const struct btree_node *n, uintptr_type value)
{
  for (unsigned index = 0, ec = n->entry_count; index != ec; ++index)
    if (n->content.children[index].separator >= value)
      return index;
  return n->entry_count;
}

// Find the position for a slot in a leaf node.
static unsigned
btree_node_find_leaf_slot (const struct btree_node *n, uintptr_type value)
{
  for (unsigned index = 0, ec = n->entry_count; index != ec; ++index)
    if (n->content.entries[index].base + n->content.entries[index].size > value)
      return index;
  return n->entry_count;
}

// Try to lock the node exclusive.
static inline bool
btree_node_try_lock_exclusive (struct btree_node *n)
{
  return version_lock_try_lock_exclusive (&(n->version_lock));
}

// Lock the node exclusive, blocking as needed.
static inline void
btree_node_lock_exclusive (struct btree_node *n)
{
  version_lock_lock_exclusive (&(n->version_lock));
}

// Release a locked node and increase the version lock.
static inline void
btree_node_unlock_exclusive (struct btree_node *n)
{
  version_lock_unlock_exclusive (&(n->version_lock));
}

// Acquire an optimistic "lock". Note that this does not lock at all, it
// only allows for validation later.
static inline bool
btree_node_lock_optimistic (const struct btree_node *n, uintptr_type *lock)
{
  return version_lock_lock_optimistic (&(n->version_lock), lock);
}

// Validate a previously acquire lock.
static inline bool
btree_node_validate (const struct btree_node *n, uintptr_type lock)
{
  return version_lock_validate (&(n->version_lock), lock);
}

// Insert a new separator after splitting.
static void
btree_node_update_separator_after_split (struct btree_node *n,
					 uintptr_type old_separator,
					 uintptr_type new_separator,
					 struct btree_node *new_right)
{
  unsigned slot = btree_node_find_inner_slot (n, old_separator);
  for (unsigned index = n->entry_count; index > slot; --index)
    n->content.children[index] = n->content.children[index - 1];
  n->content.children[slot].separator = new_separator;
  n->content.children[slot + 1].child = new_right;
  n->entry_count++;
}

// A btree. Suitable for static initialization, all members are zero at the
// beginning.
struct btree
{
  // The root of the btree.
  struct btree_node *root;
  // The free list of released node.
  struct btree_node *free_list;
  // The version lock used to protect the root.
  struct version_lock root_lock;
};

// Initialize a btree. Not actually used, just for exposition.
static inline void
btree_init (struct btree *t)
{
  t->root = NULL;
  t->free_list = NULL;
  t->root_lock.version_lock = 0;
};

static void
btree_release_tree_recursively (struct btree *t, struct btree_node *n);

// Destroy a tree and release all nodes.
static void
btree_destroy (struct btree *t)
{
  // Disable the mechanism before cleaning up.
  struct btree_node *old_root
    = __atomic_exchange_n (&(t->root), NULL, __ATOMIC_SEQ_CST);
  if (old_root)
    btree_release_tree_recursively (t, old_root);

  // Release all free nodes.
  while (t->free_list)
    {
      struct btree_node *next = t->free_list->content.children[0].child;
      free (t->free_list);
      t->free_list = next;
    }
}

// Allocate a node. This node will be returned in locked exclusive state.
static struct btree_node *
btree_allocate_node (struct btree *t, bool inner)
{
  while (true)
    {
      // Try the free list first.
      struct btree_node *next_free
	= __atomic_load_n (&(t->free_list), __ATOMIC_SEQ_CST);
      if (next_free)
	{
	  if (!btree_node_try_lock_exclusive (next_free))
	    continue;
	  // The node might no longer be free, check that again after acquiring
	  // the exclusive lock.
	  if (next_free->type == btree_node_free)
	    {
	      struct btree_node *ex = next_free;
	      if (__atomic_compare_exchange_n (
		    &(t->free_list), &ex, next_free->content.children[0].child,
		    false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
		{
		  next_free->entry_count = 0;
		  next_free->type = inner ? btree_node_inner : btree_node_leaf;
		  return next_free;
		}
	    }
	  btree_node_unlock_exclusive (next_free);
	  continue;
	}

      // No free node available, allocate a new one.
      struct btree_node *new_node
	= (struct btree_node *) (malloc (sizeof (struct btree_node)));
      version_lock_initialize_locked_exclusive (
	&(new_node->version_lock)); // initialize the node in locked state.
      new_node->entry_count = 0;
      new_node->type = inner ? btree_node_inner : btree_node_leaf;
      return new_node;
    }
}

// Release a node. This node must be currently locked exclusively and will
// be placed in the free list.
static void
btree_release_node (struct btree *t, struct btree_node *node)
{
  // We cannot release the memory immediately because there might still be
  // concurrent readers on that node. Put it in the free list instead.
  node->type = btree_node_free;
  struct btree_node *next_free
    = __atomic_load_n (&(t->free_list), __ATOMIC_SEQ_CST);
  do
    {
      node->content.children[0].child = next_free;
  } while (!__atomic_compare_exchange_n (&(t->free_list), &next_free, node,
					 false, __ATOMIC_SEQ_CST,
					 __ATOMIC_SEQ_CST));
  btree_node_unlock_exclusive (node);
}

// Recursively release a tree. The btree is by design very shallow, thus
// we can risk recursion here.
static void
btree_release_tree_recursively (struct btree *t, struct btree_node *node)
{
  btree_node_lock_exclusive (node);
  if (btree_node_is_inner (node))
    {
      for (unsigned index = 0; index < node->entry_count; ++index)
	btree_release_tree_recursively (t, node->content.children[index].child);
    }
  btree_release_node (t, node);
}

// Check if we are splitting the root.
static void
btree_handle_root_split (struct btree *t, struct btree_node **node,
			 struct btree_node **parent)
{
  // We want to keep the root pointer stable to allow for contention
  // free reads. Thus, we split the root by first moving the content
  // of the root node to a new node, and then split that new node.
  if (!*parent)
    {
      // Allocate a new node, this guarantees us that we will have a parent
      // afterwards.
      struct btree_node *new_node
	= btree_allocate_node (t, btree_node_is_inner (*node));
      struct btree_node *old_node = *node;
      new_node->entry_count = old_node->entry_count;
      new_node->content = old_node->content;
      old_node->content.children[0].separator = max_separator;
      old_node->content.children[0].child = new_node;
      old_node->entry_count = 1;
      old_node->type = btree_node_inner;

      *parent = old_node;
      *node = new_node;
    }
}

// Split an inner node.
static void
btree_split_inner (struct btree *t, struct btree_node **inner,
		   struct btree_node **parent, uintptr_type target)
{
  // Check for the root.
  btree_handle_root_split (t, inner, parent);

  // Create two inner node.
  uintptr_type right_fence = btree_node_get_fence_key (*inner);
  struct btree_node *left_inner = *inner;
  struct btree_node *right_inner = btree_allocate_node (t, true);
  unsigned split = left_inner->entry_count / 2;
  right_inner->entry_count = left_inner->entry_count - split;
  for (unsigned index = 0; index < right_inner->entry_count; ++index)
    right_inner->content.children[index]
      = left_inner->content.children[split + index];
  left_inner->entry_count = split;
  uintptr_type left_fence = btree_node_get_fence_key (left_inner);
  btree_node_update_separator_after_split (*parent, right_fence, left_fence,
					   right_inner);
  if (target <= left_fence)
    {
      *inner = left_inner;
      btree_node_unlock_exclusive (right_inner);
    }
  else
    {
      *inner = right_inner;
      btree_node_unlock_exclusive (left_inner);
    }
}

// Split a leaf node.
static void
btree_split_leaf (struct btree *t, struct btree_node **leaf,
		  struct btree_node **parent, uintptr_type fence,
		  uintptr_type target)
{
  // Check for the root.
  btree_handle_root_split (t, leaf, parent);

  // Create two leaf nodes.
  uintptr_type right_fence = fence;
  struct btree_node *left_leaf = *leaf;
  struct btree_node *right_leaf = btree_allocate_node (t, false);
  unsigned split = left_leaf->entry_count / 2;
  right_leaf->entry_count = left_leaf->entry_count - split;
  for (unsigned index = 0; index != right_leaf->entry_count; ++index)
    right_leaf->content.entries[index]
      = left_leaf->content.entries[split + index];
  left_leaf->entry_count = split;
  uintptr_type left_fence = right_leaf->content.entries[0].base - 1;
  btree_node_update_separator_after_split (*parent, right_fence, left_fence,
					   right_leaf);
  if (target <= left_fence)
    {
      *leaf = left_leaf;
      btree_node_unlock_exclusive (right_leaf);
    }
  else
    {
      *leaf = right_leaf;
      btree_node_unlock_exclusive (left_leaf);
    }
}

// Merge (or balance) child nodes.
static struct btree_node *
btree_merge_node (struct btree *t, unsigned child_slot,
		  struct btree_node *parent, uintptr_type target)
{
  // Choose the emptiest neighbor and lock both. The target child is already
  // locked.
  unsigned left_slot;
  struct btree_node *left_node, *right_node;
  if ((child_slot == 0)
      || (((child_slot + 1) < parent->entry_count)
	  && (parent->content.children[child_slot + 1].child->entry_count
	      < parent->content.children[child_slot - 1].child->entry_count)))
    {
      left_slot = child_slot;
      left_node = parent->content.children[left_slot].child;
      right_node = parent->content.children[left_slot + 1].child;
      btree_node_lock_exclusive (right_node);
    }
  else
    {
      left_slot = child_slot - 1;
      left_node = parent->content.children[left_slot].child;
      right_node = parent->content.children[left_slot + 1].child;
      btree_node_lock_exclusive (left_node);
    }

  // Can we merge both nodes into one node?
  unsigned total_count = left_node->entry_count + right_node->entry_count;
  unsigned max_count
    = btree_node_is_inner (left_node) ? max_fanout_inner : max_fanout_leaf;
  if (total_count <= max_count)
    {
      // Merge into the parent?
      if (parent->entry_count == 2)
	{
	  // Merge children into parent. This can only happen at the root.
	  if (btree_node_is_inner (left_node))
	    {
	      for (unsigned index = 0; index != left_node->entry_count; ++index)
		parent->content.children[index]
		  = left_node->content.children[index];
	      for (unsigned index = 0; index != right_node->entry_count;
		   ++index)
		parent->content.children[index + left_node->entry_count]
		  = right_node->content.children[index];
	    }
	  else
	    {
	      parent->type = btree_node_leaf;
	      for (unsigned index = 0; index != left_node->entry_count; ++index)
		parent->content.entries[index]
		  = left_node->content.entries[index];
	      for (unsigned index = 0; index != right_node->entry_count;
		   ++index)
		parent->content.entries[index + left_node->entry_count]
		  = right_node->content.entries[index];
	    }
	  parent->entry_count = total_count;
	  btree_release_node (t, left_node);
	  btree_release_node (t, right_node);
	  return parent;
	}
      else
	{
	  // Regular merge.
	  if (btree_node_is_inner (left_node))
	    {
	      for (unsigned index = 0; index != right_node->entry_count;
		   ++index)
		left_node->content.children[left_node->entry_count++]
		  = right_node->content.children[index];
	    }
	  else
	    {
	      for (unsigned index = 0; index != right_node->entry_count;
		   ++index)
		left_node->content.entries[left_node->entry_count++]
		  = right_node->content.entries[index];
	    }
	  parent->content.children[left_slot].separator
	    = parent->content.children[left_slot + 1].separator;
	  for (unsigned index = left_slot + 1; index + 1 < parent->entry_count;
	       ++index)
	    parent->content.children[index]
	      = parent->content.children[index + 1];
	  parent->entry_count--;
	  btree_release_node (t, right_node);
	  btree_node_unlock_exclusive (parent);
	  return left_node;
	}
    }

  // No merge possible, rebalance instead.
  if (left_node->entry_count > right_node->entry_count)
    {
      // Shift from left to right.
      unsigned to_shift
	= (left_node->entry_count - right_node->entry_count) / 2;
      if (btree_node_is_inner (left_node))
	{
	  for (unsigned index = 0; index != right_node->entry_count; ++index)
	    {
	      unsigned pos = right_node->entry_count - 1 - index;
	      right_node->content.children[pos + to_shift]
		= right_node->content.children[pos];
	    }
	  for (unsigned index = 0; index != to_shift; ++index)
	    right_node->content.children[index]
	      = left_node->content
		  .children[left_node->entry_count - to_shift + index];
	}
      else
	{
	  for (unsigned index = 0; index != right_node->entry_count; ++index)
	    {
	      unsigned pos = right_node->entry_count - 1 - index;
	      right_node->content.entries[pos + to_shift]
		= right_node->content.entries[pos];
	    }
	  for (unsigned index = 0; index != to_shift; ++index)
	    right_node->content.entries[index]
	      = left_node->content
		  .entries[left_node->entry_count - to_shift + index];
	}
      left_node->entry_count -= to_shift;
      right_node->entry_count += to_shift;
    }
  else
    {
      // Shift from right to left.
      unsigned to_shift
	= (right_node->entry_count - left_node->entry_count) / 2;
      if (btree_node_is_inner (left_node))
	{
	  for (unsigned index = 0; index != to_shift; ++index)
	    left_node->content.children[left_node->entry_count + index]
	      = right_node->content.children[index];
	  for (unsigned index = 0; index != right_node->entry_count - to_shift;
	       ++index)
	    right_node->content.children[index]
	      = right_node->content.children[index + to_shift];
	}
      else
	{
	  for (unsigned index = 0; index != to_shift; ++index)
	    left_node->content.entries[left_node->entry_count + index]
	      = right_node->content.entries[index];
	  for (unsigned index = 0; index != right_node->entry_count - to_shift;
	       ++index)
	    right_node->content.entries[index]
	      = right_node->content.entries[index + to_shift];
	}
      left_node->entry_count += to_shift;
      right_node->entry_count -= to_shift;
    }
  uintptr_type left_fence;
  if (btree_node_is_leaf (left_node))
    {
      left_fence = right_node->content.entries[0].base - 1;
    }
  else
    {
      left_fence = btree_node_get_fence_key (left_node);
    }
  parent->content.children[left_slot].separator = left_fence;
  btree_node_unlock_exclusive (parent);
  if (target <= left_fence)
    {
      btree_node_unlock_exclusive (right_node);
      return left_node;
    }
  else
    {
      btree_node_unlock_exclusive (left_node);
      return right_node;
    }
}

// Insert an entry.
static bool
btree_insert (struct btree *t, uintptr_type base, uintptr_type size,
	      struct object *ob)
{
  // Sanity check.
  if (!size)
    return false;

  // Access the root.
  struct btree_node *iter, *parent = NULL;
  {
    version_lock_lock_exclusive (&(t->root_lock));
    iter = t->root;
    if (iter)
      {
	btree_node_lock_exclusive (iter);
      }
    else
      {
	t->root = iter = btree_allocate_node (t, false);
      }
    version_lock_unlock_exclusive (&(t->root_lock));
  }

  // Walk down the btree with classic lock coupling and eager splits.
  // Strictly speaking this is not performance optimal, we could use
  // optimistic lock coupling until we hit a node that has to be modified.
  // But that is more difficult to implement and frame registration is
  // rare anyway, we use simple locking for now.

  uintptr_type fence = max_separator;
  while (btree_node_is_inner (iter))
    {
      // Use eager splits to avoid lock coupling up.
      if (iter->entry_count == max_fanout_inner)
	btree_split_inner (t, &iter, &parent, base);

      unsigned slot = btree_node_find_inner_slot (iter, base);
      if (parent)
	btree_node_unlock_exclusive (parent);
      parent = iter;
      fence = iter->content.children[slot].separator;
      iter = iter->content.children[slot].child;
      btree_node_lock_exclusive (iter);
    }

  // Make sure we have space.
  if (iter->entry_count == max_fanout_leaf)
    btree_split_leaf (t, &iter, &parent, fence, base);
  if (parent)
    btree_node_unlock_exclusive (parent);

  // Insert in node.
  unsigned slot = btree_node_find_leaf_slot (iter, base);
  if ((slot < iter->entry_count) && (iter->content.entries[slot].base == base))
    {
      // Duplicate entry, this should never happen.
      btree_node_unlock_exclusive (iter);
      return false;
    }
  for (unsigned index = iter->entry_count; index > slot; --index)
    iter->content.entries[index] = iter->content.entries[index - 1];
  struct leaf_entry *e = &(iter->content.entries[slot]);
  e->base = base;
  e->size = size;
  e->ob = ob;
  iter->entry_count++;
  btree_node_unlock_exclusive (iter);
  return true;
}

// Remove an entry.
static struct object *
btree_remove (struct btree *t, uintptr_type base)
{
  // Access the root.
  version_lock_lock_exclusive (&(t->root_lock));
  struct btree_node *iter = t->root;
  if (iter)
    btree_node_lock_exclusive (iter);
  version_lock_unlock_exclusive (&(t->root_lock));
  if (!iter)
    return NULL;

  // Same strategy as with insert, walk down with lock coupling and
  // merge eagerly.
  while (btree_node_is_inner (iter))
    {
      unsigned slot = btree_node_find_inner_slot (iter, base);
      struct btree_node *next = iter->content.children[slot].child;
      btree_node_lock_exclusive (next);
      if (btree_node_needs_merge (next))
	{
	  // Use eager merges to avoid lock coupling up.
	  iter = btree_merge_node (t, slot, iter, base);
	}
      else
	{
	  btree_node_unlock_exclusive (iter);
	  iter = next;
	}
    }

  // Remove existing entry.
  unsigned slot = btree_node_find_leaf_slot (iter, base);
  if ((slot >= iter->entry_count) || (iter->content.entries[slot].base != base))
    {
      // Not found, this should never happen.
      btree_node_unlock_exclusive (iter);
      return NULL;
    }
  struct object *ob = iter->content.entries[slot].ob;
  for (unsigned index = slot; index + 1 < iter->entry_count; ++index)
    iter->content.entries[index] = iter->content.entries[index + 1];
  iter->entry_count--;
  btree_node_unlock_exclusive (iter);
  return ob;
}

// Find the corresponding entry for the given address.
static struct object *
btree_lookup (const struct btree *t, uintptr_type target_addr)
{
  // Within this function many loads are relaxed atomic loads.
  // Use a macro to keep the code reasonable.
#define RLOAD(x) __atomic_load_n (&(x), __ATOMIC_RELAXED)

  // For targets where unwind info is usually not registered through these
  // APIs anymore, avoid any sequential consistent atomics.
  // Use relaxed MO here, it is up to the app to ensure that the library
  // loading/initialization happens-before using that library in other
  // threads (in particular unwinding with that library's functions
  // appearing in the backtraces).  Calling that library's functions
  // without waiting for the library to initialize would be racy.
  if (__builtin_expect (!RLOAD (t->root), 1))
    return NULL;

  // The unwinding tables are mostly static, they only change when
  // frames are added or removed. This makes it extremely unlikely that they
  // change during a given unwinding sequence. Thus, we optimize for the
  // contention free case and use optimistic lock coupling. This does not
  // require any writes to shared state, instead we validate every read. It is
  // important that we do not trust any value that we have read until we call
  // validate again. Data can change at arbitrary points in time, thus we always
  // copy something into a local variable and validate again before acting on
  // the read. In the unlikely event that we encounter a concurrent change we
  // simply restart and try again.

restart:
  struct btree_node *iter;
  uintptr_type lock;
  {
    // Accessing the root node requires defending against concurrent pointer
    // changes Thus we couple rootLock -> lock on root node -> validate rootLock
    if (!version_lock_lock_optimistic (&(t->root_lock), &lock))
      goto restart;
    iter = RLOAD (t->root);
    if (!version_lock_validate (&(t->root_lock), lock))
      goto restart;
    if (!iter)
      return NULL;
    uintptr_type child_lock;
    if ((!btree_node_lock_optimistic (iter, &child_lock))
	|| (!version_lock_validate (&(t->root_lock), lock)))
      goto restart;
    lock = child_lock;
  }

  // Now we can walk down towards the right leaf node.
  while (true)
    {
      enum node_type type = RLOAD (iter->type);
      unsigned entry_count = RLOAD (iter->entry_count);
      if (!btree_node_validate (iter, lock))
	goto restart;
      if (!entry_count)
	return NULL;

      if (type == btree_node_inner)
	{
	  // We cannot call find_inner_slot here because we need (relaxed)
	  // atomic reads here.
	  unsigned slot = 0;
	  while (
	    ((slot + 1) < entry_count)
	    && (RLOAD (iter->content.children[slot].separator) < target_addr))
	    ++slot;
	  struct btree_node *child = RLOAD (iter->content.children[slot].child);
	  if (!btree_node_validate (iter, lock))
	    goto restart;

	  // The node content can change at any point in time, thus we must
	  // interleave parent and child checks.
	  uintptr_type child_lock;
	  if (!btree_node_lock_optimistic (child, &child_lock))
	    goto restart;
	  if (!btree_node_validate (iter, lock))
	    goto restart; // make sure we still point to the correct node after
			  // acquiring the optimistic lock.

	  // Go down
	  iter = child;
	  lock = child_lock;
	}
      else
	{
	  // We cannot call find_leaf_slot here because we need (relaxed)
	  // atomic reads here.
	  unsigned slot = 0;
	  while (((slot + 1) < entry_count)
		 && (RLOAD (iter->content.entries[slot].base)
		       + RLOAD (iter->content.entries[slot].size)
		     <= target_addr))
	    ++slot;
	  struct leaf_entry entry;
	  entry.base = RLOAD (iter->content.entries[slot].base);
	  entry.size = RLOAD (iter->content.entries[slot].size);
	  entry.ob = RLOAD (iter->content.entries[slot].ob);
	  if (!btree_node_validate (iter, lock))
	    goto restart;

	  // Check if we have a hit.
	  if ((entry.base <= target_addr)
	      && (target_addr < entry.base + entry.size))
	    {
	      return entry.ob;
	    }
	  return NULL;
	}
    }
#undef RLOAD
}

#endif /* unwind-dw2-btree.h */
