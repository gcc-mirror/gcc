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
 * @file debug_fn_imps.hpp
 * Contains an implementation class for bin_search_tree_.
 */

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_valid(bool check_iterators, bool check_metadata) const
{
  PB_ASSOC_DBG_ASSERT(m_p_head != NULL);

  if (m_p_head->m_p_parent == NULL)
    {
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_left == m_p_head);
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_right == m_p_head);

      if (check_metadata)
	PB_ASSOC_DBG_ASSERT(m_size == 0);
    }
  else
    {
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_parent->m_p_parent == m_p_head);

      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_left != m_p_head);
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_right != m_p_head);

      if (check_metadata)
	PB_ASSOC_DBG_ASSERT(m_size > 0);
    }

  if (check_metadata)
    assert_size();

  if (m_p_head->m_p_parent != NULL)
    assert_node_consistent(m_p_head->m_p_parent);

  assert_min();
  assert_max();

  if (check_metadata)
    assert_consistent_with_debug_base();

  if (check_iterators&&  check_metadata)
    assert_iterators();
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_node_consistent_with_left(const node_pointer p_nd) const
{
  if (p_nd->m_p_left == NULL)
    return;

  PB_ASSOC_DBG_ASSERT(p_nd->m_p_left->m_p_parent == p_nd);

  PB_ASSOC_DBG_ASSERT(!Cmp_Fn::operator()(
					  PB_ASSOC_V2F(p_nd->m_value),
					  PB_ASSOC_V2F(p_nd->m_p_left->m_value)));
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_node_consistent_with_right(const node_pointer p_nd) const
{
  if (p_nd->m_p_right == NULL)
    return;

  PB_ASSOC_DBG_ASSERT(p_nd->m_p_right->m_p_parent == p_nd);

  PB_ASSOC_DBG_ASSERT(!Cmp_Fn::operator()(
					  PB_ASSOC_V2F(p_nd->m_p_right->m_value),
					  PB_ASSOC_V2F(p_nd->m_value)));
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_min() const
{
  assert_min_imp(m_p_head->m_p_parent);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_min_imp(const node_pointer p_nd) const
{
  if (p_nd == NULL)
    {
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_left == m_p_head);

      return;
    }

  if (p_nd->m_p_left == NULL)
    {
      PB_ASSOC_DBG_ASSERT(p_nd == m_p_head->m_p_left);

      return;
    }

  assert_min_imp(p_nd->m_p_left);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_max() const
{
  assert_max_imp(m_p_head->m_p_parent);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_max_imp(const node_pointer p_nd) const
{
  if (p_nd == NULL)
    {
      PB_ASSOC_DBG_ASSERT(m_p_head->m_p_right == m_p_head);

      return;
    }

  if (p_nd->m_p_right == NULL)
    {
      PB_ASSOC_DBG_ASSERT(p_nd == m_p_head->m_p_right);

      return;
    }

  assert_max_imp(p_nd->m_p_right);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_iterators() const
{
  PB_ASSOC_DBG_ASSERT(m_end_it.m_p_nd == m_p_head);
  PB_ASSOC_DBG_ASSERT(m_rend_it.m_p_nd == m_p_head);

  size_type iterated_num = 0;

  const_iterator prev_it = end();

  for (const_iterator it = begin(); it != end(); ++it)
    {
      ++iterated_num;

      PB_ASSOC_DBG_ASSERT(lower_bound(
				      PB_ASSOC_V2F(*it)).m_p_nd == it.m_p_nd);

      const_iterator upper_bound_it = upper_bound(
						  PB_ASSOC_V2F(*it));

      --upper_bound_it;

      PB_ASSOC_DBG_ASSERT(upper_bound_it.m_p_nd == it.m_p_nd);

      if (prev_it != end())
	PB_ASSOC_DBG_ASSERT(Cmp_Fn::operator()(
					       PB_ASSOC_V2F(*prev_it),
					       PB_ASSOC_V2F(*it)));

      prev_it = it;
    }

  PB_ASSOC_DBG_ASSERT(iterated_num == m_size);

  size_type reverse_iterated_num = 0;

  const_reverse_iterator reverse_prev_it = rend();

  for (const_reverse_iterator reverse_it = rbegin(); reverse_it != rend();
       ++reverse_it)
    {
      ++reverse_iterated_num;

      PB_ASSOC_DBG_ASSERT(lower_bound(
				      PB_ASSOC_V2F(*reverse_it)).m_p_nd == reverse_it.m_p_nd);

      const_iterator upper_bound_it = upper_bound(
						  PB_ASSOC_V2F(*reverse_it));

      --upper_bound_it;

      PB_ASSOC_DBG_ASSERT(upper_bound_it.m_p_nd == reverse_it.m_p_nd);

      if (reverse_prev_it != rend())
	PB_ASSOC_DBG_ASSERT(!Cmp_Fn::operator()(
						PB_ASSOC_V2F(*reverse_prev_it),
						PB_ASSOC_V2F(*reverse_it)));

      reverse_prev_it = reverse_it;
    }

  PB_ASSOC_DBG_ASSERT(reverse_iterated_num == m_size);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_consistent_with_debug_base() const
{
  my_map_debug_base::check_size(m_size);

  assert_consistent_with_debug_base(m_p_head->m_p_parent);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_consistent_with_debug_base(const node_pointer p_nd) const
{
  if (p_nd == NULL)
    return;

  my_map_debug_base::check_key_exists(
				      PB_ASSOC_V2F(p_nd->m_value));

  assert_consistent_with_debug_base(p_nd->m_p_left);
  assert_consistent_with_debug_base(p_nd->m_p_right);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_size() const
{
  PB_ASSOC_DBG_ASSERT(recursive_count(m_p_head->m_p_parent) == m_size);
}

#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
