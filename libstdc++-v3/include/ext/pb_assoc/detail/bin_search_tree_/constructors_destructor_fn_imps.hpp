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
 * @file constructors_destructor_fn_imps.hpp
 * Contains an implementation class for bin_search_tree_.
 */

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::node_allocator
PB_ASSOC_CLASS_C_DEC::s_node_allocator;

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME() :
  m_p_head(s_node_allocator.allocate(1)),
  m_end_it(m_p_head),
  m_rend_it(m_p_head),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn) :
  Cmp_Fn(r_cmp_fn),
  m_p_head(s_node_allocator.allocate(1)),
  m_end_it(m_p_head),
  m_rend_it(m_p_head),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const Node_Updator& r_node_updator) :
  Cmp_Fn(r_cmp_fn),
  Node_Updator(r_node_updator),
  m_p_head(s_node_allocator.allocate(1)),
  m_end_it(m_p_head),
  m_rend_it(m_p_head),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other) :
#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
  my_map_debug_base(r_other),
#endif // #ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
  Cmp_Fn(r_other),
  Node_Updator(r_other),
  m_p_head(s_node_allocator.allocate(1)),
  m_end_it(m_p_head),
  m_rend_it(m_p_head),
  m_size(0)
{
  initialize();

  m_size = r_other.m_size;

  PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

    try
      {
	m_p_head->m_p_parent =
	  recursive_copy_node(r_other.m_p_head->m_p_parent);

	if (m_p_head->m_p_parent != NULL)
	  m_p_head->m_p_parent->m_p_parent = m_p_head;

	m_size = r_other.m_size;

	initialize_min_max();
      }
    catch(...)
      {
	PB_ASSOC_DBG_ONLY(my_map_debug_base::clear();)

	  s_node_allocator.deallocate(m_p_head, 1);

	throw;
      }

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

    PB_ASSOC_DBG_ONLY(my_map_debug_base::swap(r_other);)

    std::swap(m_p_head, r_other.m_p_head);

  std::swap(m_size, r_other.m_size);

  std::swap(m_end_it, r_other.m_end_it);

  std::swap(m_rend_it, r_other.m_rend_it);

  std::swap((Cmp_Fn& )(*this), (Cmp_Fn& )r_other);

  Node_Updator::swap(r_other);

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~PB_ASSOC_CLASS_NAME()
{
  clear();

  s_node_allocator.deallocate(m_p_head, 1);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
initialize()
{
  m_p_head->m_p_parent = NULL;
  m_p_head->m_p_left = m_p_head;
  m_p_head->m_p_right = m_p_head;

  m_size = 0;
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::node_pointer
PB_ASSOC_CLASS_C_DEC::
recursive_copy_node(const node_pointer p_nd)
{
  if (p_nd == NULL)
    return (NULL);

  node_pointer p_ret = s_node_allocator.allocate(1);

  try
    {
      new (p_ret) node(*p_nd);
    }
  catch(...)
    {
      s_node_allocator.deallocate(p_ret, 1);

      throw;
    }

  p_ret->m_p_left = p_ret->m_p_right = NULL;

  try
    {
      p_ret->m_p_left = recursive_copy_node(p_nd->m_p_left);

      p_ret->m_p_right = recursive_copy_node(p_nd->m_p_right);
    }
  catch(...)
    {
      clear_imp(p_ret);

      throw;
    }

  if (p_ret->m_p_left != NULL)
    p_ret->m_p_left->m_p_parent = p_ret;

  if (p_ret->m_p_right != NULL)
    p_ret->m_p_right->m_p_parent = p_ret;

  PB_ASSOC_DBG_ONLY(assert_node_consistent(p_ret);)

    return (p_ret);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
initialize_min_max()
{
  if (m_p_head->m_p_parent == NULL)
    {
      m_p_head->m_p_left = m_p_head->m_p_right = m_p_head;

      return;
    }

  {
    node_pointer p_min = m_p_head->m_p_parent;

    while (p_min->m_p_left != NULL)
      p_min = p_min->m_p_left;

    m_p_head->m_p_left = p_min;
  }

  {
    node_pointer p_max = m_p_head->m_p_parent;

    while (p_max->m_p_right != NULL)
      p_max = p_max->m_p_right;

    m_p_head->m_p_right = p_max;
  }
}

