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
 * Contains an implementation class for ov_tree_.
 */

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
split(const_key_reference r_key, PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

    if (m_size == 0)
      {
	r_other.clear();

	PB_ASSOC_DBG_ONLY(assert_valid();)
	  PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

	  return;
      }

  if (Cmp_Fn::operator()(r_key, PB_ASSOC_V2F(*begin())))
    {
      swap(r_other);

      PB_ASSOC_DBG_ONLY(assert_valid();)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

	return;
    }

  if (!Cmp_Fn::operator()(
			  r_key,
			  PB_ASSOC_V2F(*(end() - 1))))
    {
      PB_ASSOC_DBG_ONLY(assert_valid();)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

	return;
    }

  if (m_size == 1)
    {
      swap(r_other);

      PB_ASSOC_DBG_ONLY(assert_valid();)
	PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

	return;
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

  iterator it = upper_bound(r_key);

  PB_ASSOC_CLASS_C_DEC new_other(r_other, r_other);

  new_other.copy_from_ordered_range(it, end());

  PB_ASSOC_CLASS_C_DEC new_this(*this, * this);

  new_this.copy_from_ordered_range(begin(), it);

  // No exceptions from this point.

  r_other.update(r_other.node_begin(), (Node_Updator* )(&r_other));

  update(node_begin(), (Node_Updator* )this);

  r_other.swap(new_other);

  swap(new_this);

  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
join(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

    if (r_other.m_size == 0)
      return;

  if (m_size == 0)
    {
      swap(r_other);

      return;
    }

  const bool greater = Cmp_Fn::operator()(
					  PB_ASSOC_V2F(*(end() - 1)),
					  PB_ASSOC_V2F(*r_other.begin()));

  const bool lesser = Cmp_Fn::operator()(
					 PB_ASSOC_V2F(*(r_other.end() - 1)),
					 PB_ASSOC_V2F(*begin()));

  if (!greater&&  !lesser)
    throw cannot_join();

#ifdef PB_ASSOC_BIN_SEARCH_TREE_DEBUG_
  for (const_iterator other_it = r_other.begin(); other_it != r_other.end();
       ++other_it)
    {
      my_map_debug_base::insert_new(PB_ASSOC_V2F(*other_it));
      r_other.my_map_debug_base::erase_existing(PB_ASSOC_V2F(*other_it));
    }
#endif // PB_ASSOC_BIN_SEARCH_TREE_DEBUG_

  PB_ASSOC_CLASS_C_DEC new_this(*this, * this);

  if (greater)
    new_this.copy_from_ordered_range(
				     begin(),
				     end(),
				     r_other.begin(),
				     r_other.end());
  else
    new_this.copy_from_ordered_range(
				     r_other.begin(),
				     r_other.end(),
				     begin(),
				     end());

  // No exceptions from this point.

  swap(new_this);

  r_other.clear();

  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    }
