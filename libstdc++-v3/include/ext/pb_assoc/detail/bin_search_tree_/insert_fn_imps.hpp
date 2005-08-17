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

/*
 * @file insert_fn_imps.hpp
 * Contains an implementation class for bin_search_tree_.
 */

PB_ASSOC_CLASS_T_DEC
inline std::pair<typename PB_ASSOC_CLASS_C_DEC::find_iterator, bool>
PB_ASSOC_CLASS_C_DEC::
insert_leaf(const_mapped_reference r_mapped_value)
{
  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)

    if (m_size == 0)
      return (std::make_pair(
			     insert_imp_empty(r_mapped_value),
			     true));

  node_pointer p_nd = m_p_head->m_p_parent;
  node_pointer p_pot = m_p_head;

  while (p_nd != NULL)
    if (!Cmp_Fn::operator()(
			    PB_ASSOC_V2F(p_nd->m_value),
			    PB_ASSOC_V2F(r_mapped_value)))
      {
	p_pot = p_nd;

	p_nd = p_nd->m_p_left;
      }
    else
      p_nd = p_nd->m_p_right;

  if (p_pot == m_p_head)
    return (std::make_pair(
			   insert_leaf_new(r_mapped_value, m_p_head->m_p_right, false),
			   true));

  if (!Cmp_Fn::operator()(
			  PB_ASSOC_V2F(r_mapped_value),
			  PB_ASSOC_V2F(p_pot->m_value)))
    {
      PB_ASSOC_DBG_ONLY(assert_valid(true, true);)

	PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(
							      PB_ASSOC_V2F(r_mapped_value)));

      return (std::make_pair(p_pot, false));
    }

  PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(
								PB_ASSOC_V2F(r_mapped_value)));

  p_nd = p_pot->m_p_left;
  if (p_nd == NULL)
    return (std::make_pair(
			   insert_leaf_new(r_mapped_value, p_pot, true),
			   true));

  while (p_nd->m_p_right != NULL)
    p_nd = p_nd->m_p_right;

  return (std::make_pair(
			 insert_leaf_new(r_mapped_value, p_nd, false),
			 true));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::iterator
PB_ASSOC_CLASS_C_DEC::
insert_leaf_new(const_mapped_reference r_mapped_value, node_pointer p_nd, bool left_nd)
{
  node_pointer p_new_nd =
    get_new_node_for_leaf_insert(r_mapped_value, my_traits_base::s_no_throw_copies_indicator);

  if (left_nd)
    {
      PB_ASSOC_DBG_ASSERT(p_nd->m_p_left == NULL);
      PB_ASSOC_DBG_ASSERT(Cmp_Fn::operator()(
					     PB_ASSOC_V2F(r_mapped_value),
					     PB_ASSOC_V2F(p_nd->m_value)));

      p_nd->m_p_left = p_new_nd;

      if (m_p_head->m_p_left == p_nd)
	m_p_head->m_p_left = p_new_nd;
    }
  else
    {
      PB_ASSOC_DBG_ASSERT(p_nd->m_p_right == NULL);
      PB_ASSOC_DBG_ASSERT(Cmp_Fn::operator()(
					     PB_ASSOC_V2F(p_nd->m_value),
					     PB_ASSOC_V2F(r_mapped_value)));

      p_nd->m_p_right = p_new_nd;

      if (m_p_head->m_p_right == p_nd)
	m_p_head->m_p_right = p_new_nd;
    }

  p_new_nd->m_p_parent = p_nd;

  p_new_nd->m_p_left = p_new_nd->m_p_right = NULL;

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_nd));

  update_to_top(p_new_nd, (Node_Updator* )this);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::insert_new(
						  PB_ASSOC_V2F(r_mapped_value)));

  return (iterator(p_new_nd));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::iterator
PB_ASSOC_CLASS_C_DEC::
insert_imp_empty(const_mapped_reference r_mapped_value)
{
  node_pointer p_new_node =
    get_new_node_for_leaf_insert(r_mapped_value, my_traits_base::s_no_throw_copies_indicator);

  m_p_head->m_p_left = m_p_head->m_p_right =
    m_p_head->m_p_parent = p_new_node;

  p_new_node->m_p_parent = m_p_head;

  p_new_node->m_p_left = p_new_node->m_p_right = NULL;

  PB_ASSOC_DBG_ONLY(my_map_debug_base::insert_new(
						  PB_ASSOC_V2F(r_mapped_value)));

  update_to_top(m_p_head->m_p_parent, (Node_Updator* )this);

  return (iterator(p_new_node));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::node_pointer
PB_ASSOC_CLASS_C_DEC::
get_new_node_for_leaf_insert(const_mapped_reference r_val, pb_assoc::detail::int_to_type<false>)
{
  node_pointer p_new_nd = s_node_allocator.allocate(1);

  cond_dealtor_t cond(p_new_nd);

  new (const_cast<void* >(
			  static_cast<const void* >(&p_new_nd->m_value)))
    typename Node::value_type(r_val);

  cond.set_no_action();

  p_new_nd->m_p_left = p_new_nd->m_p_right = NULL;

  ++m_size;

  return (p_new_nd);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::node_pointer
PB_ASSOC_CLASS_C_DEC::
get_new_node_for_leaf_insert(const_mapped_reference r_val, pb_assoc::detail::int_to_type<true>)
{
  node_pointer p_new_nd = s_node_allocator.allocate(1);

  new (const_cast<void* >(
			  static_cast<const void* >(&p_new_nd->m_value)))
    typename Node::value_type(r_val);

  p_new_nd->m_p_left = p_new_nd->m_p_right = NULL;

  ++m_size;

  return (p_new_nd);
}

