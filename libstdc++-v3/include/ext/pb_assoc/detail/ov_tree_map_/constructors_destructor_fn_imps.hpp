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
 * Contains an implementation class for ov_tree_.
 */

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::value_allocator
PB_ASSOC_CLASS_C_DEC::s_alloc;

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_OV_TREE_CLASS_NAME() :
  m_a_values(NULL),
  m_end_it(NULL),
  m_size(0)
{
  update(node_begin(), (Node_Updator* )this);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_OV_TREE_CLASS_NAME(const Cmp_Fn& r_cmp_fn) :
  my_cmp_fn_base(r_cmp_fn),
  m_a_values(NULL),
  m_end_it(NULL),
  m_size(0)
{
  update(node_begin(), (Node_Updator* )this);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_OV_TREE_CLASS_NAME(const Cmp_Fn& r_cmp_fn, const Node_Updator& r_node_updator) :
  my_cmp_fn_base(r_cmp_fn),
  Node_Updator(r_node_updator),
  m_a_values(NULL),
  m_end_it(NULL),
  m_size(0)
{
  update(node_begin(), (Node_Updator* )this);

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_OV_TREE_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other) :
  my_cmp_fn_base(r_other),
  m_a_values(NULL),
  m_end_it(NULL),
  m_size(0)
{
  copy_from_ordered_range(r_other.begin(), r_other.end());

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
template<class It>
inline void
PB_ASSOC_CLASS_C_DEC::
copy_from_range(It first_it, It last_it)
{
  enum
    {
      is_set_type = is_same_type<Data, null_data_type>::value
    };

  typedef
    typename cond_type<
    is_set_type,
    std::set<
    Key,
    Cmp_Fn,
    typename Allocator::template rebind<
    Key>::other>,
    std::map<
    Key,
    Data,
    Cmp_Fn,
    typename Allocator::template rebind<
    std::pair<const Key, Data> >::other> >::type
    map_type;

  map_type m(first_it, last_it);

  copy_from_ordered_range(m.begin(), m.end());
}

PB_ASSOC_CLASS_T_DEC
template<class It>
void
PB_ASSOC_CLASS_C_DEC::
copy_from_ordered_range(It first_it, It last_it)
{
  clear();

  const size_type size = std::distance(first_it, last_it);

  pointer a_values = s_alloc.allocate(size);

  iterator target_it = a_values;
  It source_it = first_it;
  It source_end_it = last_it;

  cond_dtor cd(a_values, target_it, size);

  while (source_it != source_end_it)
    {
      new (const_cast<void* >(
			      static_cast<const void* >(target_it)))
	value_type(*source_it++);

      ++target_it;
    }

  cd.set_no_action();

  m_a_values = a_values;

  m_size = size;

  m_end_it = m_a_values + m_size;

  update(node_begin(), (Node_Updator* )this);

#ifdef PB_ASSOC_OV_TREE_DEBUG_
  const_iterator dbg_it = m_a_values;

  while (dbg_it != m_end_it)
    {
      my_map_debug_base::insert_new(PB_ASSOC_V2F(*dbg_it));

      dbg_it++;
    }

  PB_ASSOC_CLASS_C_DEC::assert_valid();
#endif // #ifdef PB_ASSOC_OV_TREE_DEBUG_
}

PB_ASSOC_CLASS_T_DEC
template<class It>
void
PB_ASSOC_CLASS_C_DEC::
copy_from_ordered_range(It first_it, It last_it, It other_first_it, It other_last_it)
{
  clear();

  const size_type size =
    std::distance(first_it, last_it) +
    std::distance(other_first_it, other_last_it);

  pointer a_values = s_alloc.allocate(size);

  iterator target_it = a_values;
  It source_it = first_it;
  It source_end_it = last_it;

  cond_dtor cd(a_values, target_it, size);

  while (source_it != source_end_it)
    {
      new (const_cast<void* >(
			      static_cast<const void* >(target_it)))
	value_type(*source_it++);

      ++target_it;
    }

  source_it = other_first_it;
  source_end_it = other_last_it;

  while (source_it != source_end_it)
    {
      new (const_cast<void* >(
			      static_cast<const void* >(target_it)))
	value_type(*source_it++);

      ++target_it;
    }

  cd.set_no_action();

  m_a_values = a_values;

  m_size = size;

  m_end_it = m_a_values + m_size;

  update(node_begin(), (Node_Updator* )this);

#ifdef PB_ASSOC_OV_TREE_DEBUG_
  const_iterator dbg_it = m_a_values;

  while (dbg_it != m_end_it)
    {
      my_map_debug_base::insert_new(PB_ASSOC_V2F(*dbg_it));

      dbg_it++;
    }

  PB_ASSOC_CLASS_C_DEC::assert_valid();
#endif // #ifdef PB_ASSOC_OV_TREE_DEBUG_
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    std::swap(m_a_values, r_other.m_a_values);

  std::swap(m_size, r_other.m_size);

  std::swap(m_end_it, r_other.m_end_it);

  std::swap((Cmp_Fn& )(*this), (Cmp_Fn& )r_other);

  Node_Updator::swap(r_other);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::swap(r_other);)

    PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~PB_ASSOC_OV_TREE_CLASS_NAME()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    cond_dtor cd(m_a_values, m_end_it, m_size);
}
