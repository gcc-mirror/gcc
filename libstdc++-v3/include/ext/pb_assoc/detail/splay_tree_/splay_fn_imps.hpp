// -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice and
// this permission notice appear in supporting documentation. None of
// the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied warranty.

/**
 * @file splay_fn_imps.hpp
 * Contains an implementation class for splay_tree_.
 */

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
splay(node_pointer p_nd)
{
  while (p_nd->m_p_parent != PB_ASSOC_BASE_C_DEC::m_p_head)
    {
      PB_ASSOC_DBG_ONLY(PB_ASSOC_BASE_C_DEC::assert_node_consistent(p_nd);)

	if (p_nd->m_p_parent->m_p_parent ==
	    PB_ASSOC_BASE_C_DEC::m_p_head)
	  {
	    PB_ASSOC_BASE_C_DEC::rotate_parent(p_nd);

	    PB_ASSOC_DBG_ASSERT(p_nd ==
				PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent);
	  }
	else
	  {
	    const node_pointer p_parent = p_nd->m_p_parent;
	    const node_pointer p_grandparent = p_parent->m_p_parent;

#ifdef PB_ASSOC_SPLAY_TREE_DEBUG_
	    const size_type total =
	      PB_ASSOC_BASE_C_DEC::recursive_count(p_grandparent);

	    PB_ASSOC_DBG_ASSERT(total >= 3);
#endif // #ifdef PB_ASSOC_SPLAY_TREE_DEBUG_

	    if (p_parent->m_p_left == p_nd&& 
		p_grandparent->m_p_right == p_parent)
	      splay_zig_zag_left(p_nd, p_parent, p_grandparent);
	    else if (p_parent->m_p_right == p_nd&& 
		     p_grandparent->m_p_left == p_parent)
	      splay_zig_zag_right(p_nd, p_parent, p_grandparent);
	    else if (p_parent->m_p_left == p_nd&& 
		     p_grandparent->m_p_left == p_parent)
	      splay_zig_zig_left(p_nd, p_parent, p_grandparent);
	    else
	      splay_zig_zig_right(p_nd, p_parent, p_grandparent);

	    PB_ASSOC_DBG_ASSERT(total ==
				PB_ASSOC_BASE_C_DEC::recursive_count(p_nd));
	  }

      PB_ASSOC_DBG_ONLY(assert_node_consistent(p_nd);)
	}
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zig_zag_left(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent)
{
  PB_ASSOC_DBG_ASSERT(p_parent == p_nd->m_p_parent);
  PB_ASSOC_DBG_ASSERT(p_grandparent == p_parent->m_p_parent);

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_grandparent);)

    PB_ASSOC_DBG_ASSERT(p_parent->m_p_left == p_nd&& 
			p_grandparent->m_p_right == p_parent);

  splay_zz_start(p_nd, p_parent, p_grandparent);

  node_pointer p_b = p_nd->m_p_right;
  node_pointer p_c = p_nd->m_p_left;

  p_nd->m_p_right = p_parent;
  p_parent->m_p_parent = p_nd;

  p_nd->m_p_left = p_grandparent;
  p_grandparent->m_p_parent = p_nd;

  p_parent->m_p_left = p_b;
  if (p_b != NULL)
    p_b->m_p_parent = p_parent;

  p_grandparent->m_p_right = p_c;
  if (p_c != NULL)
    p_c->m_p_parent = p_grandparent;

  splay_zz_end(p_nd, p_parent, p_grandparent);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zig_zag_right(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent)
{
  PB_ASSOC_DBG_ASSERT(p_parent == p_nd->m_p_parent);
  PB_ASSOC_DBG_ASSERT(p_grandparent == p_parent->m_p_parent);

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_grandparent);)

    PB_ASSOC_DBG_ASSERT(p_parent->m_p_right == p_nd&& 
			p_grandparent->m_p_left == p_parent);

  splay_zz_start(p_nd, p_parent, p_grandparent);

  node_pointer p_b = p_nd->m_p_left;
  node_pointer p_c = p_nd->m_p_right;

  p_nd->m_p_left = p_parent;
  p_parent->m_p_parent = p_nd;

  p_nd->m_p_right = p_grandparent;
  p_grandparent->m_p_parent = p_nd;

  p_parent->m_p_right = p_b;
  if (p_b != NULL)
    p_b->m_p_parent = p_parent;

  p_grandparent->m_p_left = p_c;
  if (p_c != NULL)
    p_c->m_p_parent = p_grandparent;

  splay_zz_end(p_nd, p_parent, p_grandparent);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zig_zig_left(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent)
{
  PB_ASSOC_DBG_ASSERT(p_parent == p_nd->m_p_parent);
  PB_ASSOC_DBG_ASSERT(p_grandparent == p_parent->m_p_parent);

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_grandparent);)

    PB_ASSOC_DBG_ASSERT(p_parent->m_p_left == p_nd&& 
			p_nd->m_p_parent->m_p_parent->m_p_left == p_nd->m_p_parent);

  splay_zz_start(p_nd, p_parent, p_grandparent);

  node_pointer p_b = p_nd->m_p_right;
  node_pointer p_c = p_parent->m_p_right;

  p_nd->m_p_right = p_parent;
  p_parent->m_p_parent = p_nd;

  p_parent->m_p_right = p_grandparent;
  p_grandparent->m_p_parent = p_parent;

  p_parent->m_p_left = p_b;
  if (p_b != NULL)
    p_b->m_p_parent = p_parent;

  p_grandparent->m_p_left = p_c;
  if (p_c != NULL)
    p_c->m_p_parent = p_grandparent;

  splay_zz_end(p_nd, p_parent, p_grandparent);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zig_zig_right(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent)
{
  PB_ASSOC_DBG_ASSERT(p_parent == p_nd->m_p_parent);
  PB_ASSOC_DBG_ASSERT(p_grandparent == p_parent->m_p_parent);

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_grandparent);)

    PB_ASSOC_DBG_ASSERT(p_parent->m_p_right == p_nd&& 
			p_nd->m_p_parent->m_p_parent->m_p_right == p_nd->m_p_parent);

  splay_zz_start(p_nd, p_parent, p_grandparent);

  node_pointer p_b = p_nd->m_p_left;
  node_pointer p_c = p_parent->m_p_left;

  p_nd->m_p_left = p_parent;
  p_parent->m_p_parent = p_nd;

  p_parent->m_p_left = p_grandparent;
  p_grandparent->m_p_parent = p_parent;

  p_parent->m_p_right = p_b;
  if (p_b != NULL)
    p_b->m_p_parent = p_parent;

  p_grandparent->m_p_right = p_c;
  if (p_c != NULL)
    p_c->m_p_parent = p_grandparent;

  PB_ASSOC_BASE_C_DEC::update_to_top(
				     p_grandparent, (Node_Updator* )this);

  splay_zz_end(p_nd, p_parent, p_grandparent);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zz_start(node_pointer p_nd,
#ifdef PB_ASSOC_SPLAY_TREE_DEBUG_
	       node_pointer p_parent,
#else // #ifdef PB_ASSOC_SPLAY_TREE_DEBUG_
	       node_pointer /*p_parent*/,
#endif // #ifdef PB_ASSOC_SPLAY_TREE_DEBUG_
	       node_pointer p_grandparent)
{
  PB_ASSOC_DBG_ASSERT(p_nd != NULL);
  PB_ASSOC_DBG_ASSERT(p_parent != NULL);
  PB_ASSOC_DBG_ASSERT(p_grandparent != NULL);

  const bool grandparent_head =
    p_grandparent->m_p_parent == PB_ASSOC_BASE_C_DEC::m_p_head;

  if (grandparent_head)
    {
      PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent =
	PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent;

      p_nd->m_p_parent = PB_ASSOC_BASE_C_DEC::m_p_head;

      return;
    }

  node_pointer p_greatgrandparent = p_grandparent->m_p_parent;

  p_nd->m_p_parent = p_greatgrandparent;

  if (p_grandparent == p_greatgrandparent->m_p_left)
    p_greatgrandparent->m_p_left = p_nd;
  else
    p_greatgrandparent->m_p_right = p_nd;
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
splay_zz_end(node_pointer p_nd, node_pointer p_parent, node_pointer p_grandparent)
{
  if (p_nd->m_p_parent == PB_ASSOC_BASE_C_DEC::m_p_head)
    PB_ASSOC_BASE_C_DEC::m_p_head->m_p_parent = p_nd;

  apply_update(p_grandparent, (Node_Updator* )this);
  apply_update(p_parent, (Node_Updator* )this);
  apply_update(p_nd, (Node_Updator* )this);

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_nd);)
    }

