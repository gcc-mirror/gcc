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
 * @file insert_store_hash_fn_imps.hpp
 * Contains implementations of cc_ht_map_'s insert related functions, when the hash
 *	value is stored.
 */

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::data_reference
PB_ASSOC_CLASS_C_DEC::
subscript_imp(const_key_reference r_key, int_to_type<true>)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    comp_hash pos_hash_pair = my_ranged_hash_fn_base::operator()(r_key);

  entry_pointer p_e = m_a_p_entries[pos_hash_pair.first];

  my_resize_base::notify_insert_search_start();

  while (p_e != NULL&& 
	 !my_hash_eq_fn_base::operator()(p_e->m_value.first, p_e->m_hash, r_key, pos_hash_pair.second))
    {
      my_resize_base::notify_insert_search_collision();

      p_e = p_e->m_p_next;
    }

  my_resize_base::notify_insert_search_end();

  if (p_e != NULL)
    {
      PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key);)

	return (p_e->m_value.second);
    }

  PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(r_key);)

    return (insert_new_imp(
			   value_type(r_key, Data()), pos_hash_pair)->second);
}

PB_ASSOC_CLASS_T_DEC
inline std::pair<typename PB_ASSOC_CLASS_C_DEC::find_iterator, bool>
PB_ASSOC_CLASS_C_DEC::
insert_imp(const_reference r_val, int_to_type<true>)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    const_key_reference r_key = PB_ASSOC_V2F(r_val);

  comp_hash pos_hash_pair = my_ranged_hash_fn_base::operator()(r_key);

  entry_pointer p_e = m_a_p_entries[pos_hash_pair.first];

  my_resize_base::notify_insert_search_start();

  while (p_e != NULL&& 
	 !my_hash_eq_fn_base::operator()(
					 PB_ASSOC_V2F(p_e->m_value),
					 p_e->m_hash,
					 r_key, pos_hash_pair.second))
    {
      my_resize_base::notify_insert_search_collision();

      p_e = p_e->m_p_next;
    }

  my_resize_base::notify_insert_search_end();

  if (p_e != NULL)
    {
      PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key);)

	return (
		std::make_pair(
			       static_cast<pointer>(&p_e->m_value),
			       false));
    }

  PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(r_key);)

    return (
	    std::make_pair(
			   insert_new_imp(r_val, pos_hash_pair),
			   true));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::pointer
PB_ASSOC_CLASS_C_DEC::
insert_new_imp(const_reference r_val, comp_hash& r_pos_hash_pair)
{
  // Following lines might throw an exception.

  if (do_resize_if_needed())
    r_pos_hash_pair = my_ranged_hash_fn_base::operator()(
							 PB_ASSOC_V2F(r_val));

  entry_pointer p_e = get_entry(r_val, my_traits_base::s_no_throw_copies_indicator);

  // At this point no exceptions can be thrown.

  p_e->m_hash = r_pos_hash_pair.second;

  p_e->m_p_next = m_a_p_entries[r_pos_hash_pair.first];

  m_a_p_entries[r_pos_hash_pair.first] = p_e;

  my_resize_base::notify_inserted(++m_num_used_e);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::insert_new(r_key);)

    PB_ASSOC_DBG_ONLY(assert_valid();)

    return (static_cast<pointer>(&p_e->m_value));
}

