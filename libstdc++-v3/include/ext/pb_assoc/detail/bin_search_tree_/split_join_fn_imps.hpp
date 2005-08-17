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
 * @file split_join_fn_imps.hpp
 * Contains an implementation class for bin_search_tree_.
 */

PB_ASSOC_CLASS_T_DEC
bool
PB_ASSOC_CLASS_C_DEC::
join_prep(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

    if (r_other.m_size == 0)
      return (false);

  if (m_size == 0)
    {
      swap(r_other);

      return (false);
    }

  const bool greater = Cmp_Fn::operator()(
					  PB_ASSOC_V2F(m_p_head->m_p_right->m_value),
					  PB_ASSOC_V2F(r_other.m_p_head->m_p_left->m_value));

  const bool lesser = Cmp_Fn::operator()(
					 PB_ASSOC_V2F(r_other.m_p_head->m_p_right->m_value),
					 PB_ASSOC_V2F(m_p_head->m_p_left->m_value));

  if (!greater&&  !lesser)
    throw cannot_join();

  if (lesser)
    swap(r_other);

  m_size += r_other.m_size;

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
  for (const_iterator other_it = r_other.begin(); other_it != r_other.end();
       ++other_it)
    {
      my_map_debug_base::insert_new(PB_ASSOC_V2F(*other_it));
      r_other.my_map_debug_base::erase_existing(PB_ASSOC_V2F(*other_it));
    }
#endif // PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

  return (true);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
join_finish(PB_ASSOC_CLASS_C_DEC& r_other)
{
  initialize_min_max();

  r_other.initialize();
}

PB_ASSOC_CLASS_T_DEC
bool
PB_ASSOC_CLASS_C_DEC::
split_prep(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

    r_other.clear();

  if (m_size == 0)
    {
      PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

	return (false);
    }

  if (Cmp_Fn::operator()(r_key, PB_ASSOC_V2F(m_p_head->m_p_left->m_value)))
    {
      swap(r_other);

      PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

	return (false);
    }

  if (!Cmp_Fn::operator()(
			  r_key,
			  PB_ASSOC_V2F(m_p_head->m_p_right->m_value)))
    {
      PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

	return (false);
    }

  if (m_size == 1)
    {
      swap(r_other);

      PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)

	return (false);
    }

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
  for (const_iterator it = begin(); it != end(); ++it)
    if (Cmp_Fn::operator()(
			   r_key,
			   PB_ASSOC_V2F(*it)))
      {
	my_map_debug_base::erase_existing(PB_ASSOC_V2F(*it));
	r_other.my_map_debug_base::insert_new(PB_ASSOC_V2F(*it));
      }
#endif // PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

  return (true);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
split_finish(PB_ASSOC_CLASS_C_DEC& r_other)
{
  r_other.m_size = r_other.recursive_count(r_other.m_p_head->m_p_parent);

  r_other.initialize_min_max();

  m_size -= r_other.m_size;

  initialize_min_max();

  PB_ASSOC_DBG_ONLY(assert_valid(true, true);)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid(true, true);)
    }

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
recursive_count(node_pointer p_nd) const
{
  if (p_nd == NULL)
    return (0);

  return (1 +
	  recursive_count(p_nd->m_p_left) +
	  recursive_count(p_nd->m_p_right));
}

