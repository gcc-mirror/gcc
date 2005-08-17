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
 * @file rotate_fn_imps.hpp
 * Contains imps for rotating nodes.
 */

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
rotate_left(node_pointer p_x)
{
  node_pointer p_y = p_x->m_p_right;

  p_x->m_p_right = p_y->m_p_left;

  if (p_y->m_p_left != NULL)
    p_y->m_p_left->m_p_parent = p_x;

  p_y->m_p_parent = p_x->m_p_parent;

  if (p_x == m_p_head->m_p_parent)
    m_p_head->m_p_parent = p_y;
  else if (p_x == p_x->m_p_parent->m_p_left)
    p_x->m_p_parent->m_p_left = p_y;
  else
    p_x->m_p_parent->m_p_right = p_y;

  p_y->m_p_left = p_x;
  p_x->m_p_parent = p_y;

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_x);)
    PB_ASSOC_DBG_ONLY(assert_node_consistent(p_y);)

    apply_update(p_x, (Node_Updator* )this);
  apply_update(p_x->m_p_parent, (Node_Updator* )this);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
rotate_right(node_pointer p_x)
{
  node_pointer p_y = p_x->m_p_left;

  p_x->m_p_left = p_y->m_p_right;

  if (p_y->m_p_right != NULL)
    p_y->m_p_right->m_p_parent = p_x;

  p_y->m_p_parent = p_x->m_p_parent;

  if (p_x == m_p_head->m_p_parent)
    m_p_head->m_p_parent = p_y;
  else if (p_x == p_x->m_p_parent->m_p_right)
    p_x->m_p_parent->m_p_right = p_y;
  else
    p_x->m_p_parent->m_p_left = p_y;

  p_y->m_p_right = p_x;
  p_x->m_p_parent = p_y;

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_x);)
    PB_ASSOC_DBG_ONLY(assert_node_consistent(p_y);)

    apply_update(p_x, (Node_Updator* )this);
  apply_update(p_x->m_p_parent, (Node_Updator* )this);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
rotate_parent(node_pointer p_nd)
{
  node_pointer p_parent = p_nd->m_p_parent;

  if (p_nd == p_parent->m_p_left)
    rotate_right(p_parent);
  else
    rotate_left(p_parent);

  PB_ASSOC_DBG_ASSERT(p_parent->m_p_parent = p_nd);
  PB_ASSOC_DBG_ASSERT(p_nd->m_p_left == p_parent ||
		      p_nd->m_p_right == p_parent);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
apply_update(node_pointer /*p_nd*/, pb_assoc::null_node_updator*  /*p_updator*/)
{ }

PB_ASSOC_CLASS_T_DEC
template<class Node_Updator_>
inline void
PB_ASSOC_CLASS_C_DEC::
apply_update(node_pointer p_nd, Node_Updator_* p_updator)
{
  p_updator->operator()(
			&PB_ASSOC_V2F(p_nd->m_value),(p_nd->m_p_left == NULL)?
			NULL :
			&PB_ASSOC_V2F(p_nd->m_p_left->m_value),(p_nd->m_p_right == NULL)?
			NULL :
			&PB_ASSOC_V2F(p_nd->m_p_right->m_value));
}

PB_ASSOC_CLASS_T_DEC
template<class Node_Updator_>
inline void
PB_ASSOC_CLASS_C_DEC::
update_to_top(node_pointer p_nd, Node_Updator_* p_updator)
{
  while (p_nd != m_p_head)
    {
      apply_update(p_nd, p_updator);

      p_nd = p_nd->m_p_parent;
    }
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
update_to_top(node_pointer /*p_nd*/, pb_assoc::null_node_updator*  /*p_updator*/)
{ }

