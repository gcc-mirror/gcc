/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include "libitm_i.h"

namespace GTM HIDDEN {

void
gtm_thread::record_allocation (void *ptr, void (*free_fn)(void *))
{
  // We do not deallocate before outermost commit, so we should never have
  // an existing log entry for a new allocation.
  gtm_alloc_action *a = this->alloc_actions.insert((uintptr_t) ptr);

  a->free_fn = free_fn;
  a->free_fn_sz = 0;
  a->allocated = true;
}

void
gtm_thread::forget_allocation (void *ptr, void (*free_fn)(void *))
{
  // We do not deallocate before outermost commit, so we should never have
  // an existing log entry for a deallocation at the same address.  We may
  // have an existing entry for a matching allocation, but this is handled
  // correctly because both are complementary in that only one of these will
  // cause an action at commit or abort.
  gtm_alloc_action *a = this->alloc_actions.insert((uintptr_t) ptr);
  a->free_fn = free_fn;
  a->free_fn_sz = 0;
  a->allocated = false;
}

void
gtm_thread::forget_allocation (void *ptr, size_t sz,
			       void (*free_fn_sz)(void *, size_t))
{
  // Same as forget_allocation but with a size.
  gtm_alloc_action *a = this->alloc_actions.insert((uintptr_t) ptr);
  a->free_fn = 0;
  a->free_fn_sz = free_fn_sz;
  a->sz = sz;
  a->allocated = false;
}

namespace {
struct commit_cb_data {
  aa_tree<uintptr_t, gtm_alloc_action>* parent;
  bool revert_p;
};
}

static void
commit_allocations_2 (uintptr_t key, gtm_alloc_action *a, void *data)
{
  void *ptr = (void *)key;
  commit_cb_data *cb_data = static_cast<commit_cb_data *>(data);

  if (cb_data->revert_p)
    {
      // Roll back nested allocations, discard deallocations.
      if (a->allocated)
	{
	  if (a->free_fn_sz != 0)
	    a->free_fn_sz (ptr, a->sz);
	  else
	    a->free_fn (ptr);
	}
    }
  else
    {
      // Add allocations and deallocations to parent.
      // ??? We could eliminate a (parent) allocation that matches this
      // a deallocation, if we had support for removing all accesses
      // to this allocation from the transaction's undo and redo logs
      // (otherwise, the parent transaction's undo or redo might write to
      // data that is already shared again because of calling free()).
      // We don't have this support currently, and the benefit of this
      // optimization is unknown, so just add it to the parent.
      gtm_alloc_action* a_parent = cb_data->parent->insert(key);
      *a_parent = *a;
    }
}

static void
commit_allocations_1 (uintptr_t key, gtm_alloc_action *a, void *cb_data)
{
  void *ptr = (void *)key;
  bool revert_p = (bool) (uintptr_t) cb_data;

  if (revert_p == a->allocated)
    {
      if (a->free_fn_sz != 0)
	a->free_fn_sz (ptr, a->sz);
      else
	a->free_fn (ptr);
    }
}

/* Permanently commit allocated memory during transaction.

   REVERT_P is true if instead of committing the allocations, we want
   to roll them back (and vice versa).  */
void
gtm_thread::commit_allocations (bool revert_p,
    aa_tree<uintptr_t, gtm_alloc_action>* parent)
{
  if (parent)
    {
      commit_cb_data cb_data;
      cb_data.parent = parent;
      cb_data.revert_p = revert_p;
      this->alloc_actions.traverse (commit_allocations_2, &cb_data);
    }
  else
    this->alloc_actions.traverse (commit_allocations_1,
				  (void *)(uintptr_t)revert_p);
  this->alloc_actions.clear ();
}

} // namespace GTM
