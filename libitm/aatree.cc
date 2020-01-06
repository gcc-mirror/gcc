/* Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

// Implements an AA tree (http://en.wikipedia.org/wiki/AA_tree) with an
// integer key, and data attached to the node via flexible array member.

#include "libitm_i.h"

namespace GTM HIDDEN {

// The code for rebalancing the tree is greatly simplified by never
// having to check for null pointers.  Instead, leaf node links point
// to this node, NIL, which points to itself.
const aa_node_base aa_node_base::s_nil(0);


// Remove left horizontal links.  Swap the pointers of horizontal left links.

aa_node_base *
aa_node_base::skew ()
{
  aa_node_base *l = this->link(L);
  if (this->m_level != 0 && l->m_level == this->m_level)
    {
      this->set_link(L, l->link(R));
      l->set_link(R, this);
      return l;
    }
  return this;
}


// Remove consecutive horizontal links.  Take the middle node,
// elevate it, and return it.

aa_node_base *
aa_node_base::split ()
{
  aa_node_base *r = this->link(R);
  if (this->m_level != 0 && r->link(R)->m_level == this->m_level)
    {
      this->set_link(R, r->link(L));
      r->set_link(L, this);
      r->m_level += 1;
      return r;
    }
  return this;
}

// Decrease the level of THIS to be one more than the level of its children.

void
aa_node_base::decrease_level ()
{
  aa_node_base *l = this->link(L);
  aa_node_base *r = this->link(R);
  level_type llev = l->m_level;
  level_type rlev = r->m_level;
  level_type should_be = (llev < rlev ? llev : rlev) + 1;

  if (should_be < this->m_level)
    {
      this->m_level = should_be;
      if (should_be < rlev)
	r->m_level = should_be;
    }
}

// Find and return the node in the tree with key K.

template<typename KEY>
typename aa_tree_key<KEY>::node_ptr
aa_tree_key<KEY>::find(KEY k) const
{
  node_ptr t = m_tree;
  if (t != 0)
    do
      {
	if (t->key == k)
	  return t;
	t = t->link(k > t->key);
      }
    while (!t->is_nil());
  return 0;
}

// Insert N into T and rebalance.  Return the new balanced tree.

template<typename KEY>
typename aa_tree_key<KEY>::node_ptr
aa_tree_key<KEY>::insert_1 (node_ptr t, node_ptr n)
{
  bool dir = n->key > t->key;
  node_ptr c = t->link(dir);

  // Insert the node, recursively.
  if (c->is_nil())
    c = n;
  else
    c = insert_1 (c, n);
  t->set_link(dir, c);

  // Rebalance the tree, as needed.
  t = t->skew();
  t = t->split();

  return t;
}

template<typename KEY>
void
aa_tree_key<KEY>::insert(node_ptr n)
{
  if (m_tree == 0)
    m_tree = n;
  else
    m_tree = insert_1 (m_tree, n);
}

// Delete K from T and rebalance.  Return the new balanced tree.

template<typename KEY>
typename aa_tree_key<KEY>::node_ptr
aa_tree_key<KEY>::erase_1 (node_ptr t, KEY k, node_ptr *pfree)
{
  node_ptr r;
  bool dir;

  // If this is the node we're looking for, delete it.  Else recurse.
  if (k == t->key)
    {
      node_ptr l, sub, end;

      l = t->link(node::L);
      r = t->link(node::R);

      if (pfree)
	*pfree = t;

      // If this is a leaf node, simply remove the node.  Otherwise,
      // we have to find either a predecessor or a successor node to
      // replace this one.
      if (l->is_nil())
	{
	  if (r->is_nil())
	    return r;
	  sub = r, dir = node::L;
	}
      else
	sub = l, dir = node::R;

      // Find the successor or predecessor.
      for (end = sub; !end->link(dir)->is_nil(); end = end->link(dir))
	continue;

      // Remove it (but don't free) from the subtree.
      sub = erase_1 (sub, end->key, 0);

      // Replace T with the successor we just extracted.
      end->set_link(!dir, sub);
      t = end;
    }
  else
    {
      dir = k > t->key;
      t->set_link(dir, erase_1 (t->link(dir), k, pfree));
    }

  // Rebalance the tree.
  t->decrease_level();
  t = t->skew();
  r = t->link(node::R)->skew();
  t->set_link(node::R, r);
  r->set_link(node::R, r->link(node::R)->skew());
  t = t->split ();
  t->set_link(node::R, t->link(node::R)->split());

  return t;
}

template<typename KEY>
typename aa_tree_key<KEY>::node_ptr
aa_tree_key<KEY>::erase (KEY k)
{
  node_ptr t = m_tree;
  if (t == 0)
    return 0;

  node_ptr do_free = 0;
  t = erase_1 (t, k, &do_free);
  if (t->is_nil())
    t = 0;
  m_tree = t;
  return do_free;
}

// Instantiate key classes.

template class aa_tree_key<uintptr_t>;

} // namespace GTM
