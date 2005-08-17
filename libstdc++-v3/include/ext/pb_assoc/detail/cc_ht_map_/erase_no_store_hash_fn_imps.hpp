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
 * @file erase_no_store_hash_fn_imps.hpp
 * Contains implementations of cc_ht_map_'s erase related functions, when the hash
 *	value is not stored.
 */

PB_ASSOC_CLASS_T_DEC
template<class T>
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
erase(T r_t, bool erase_entry_if_last, pb_assoc::detail::int_to_type<false>)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (erase_in_pos_imp<T>(r_t,
				erase_entry_if_last,
				my_ranged_hash_fn_base::operator()(
								   my_traits_base::ext_eraser::extract_key(r_t))));
}

PB_ASSOC_CLASS_T_DEC
template<class T>
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
erase_in_pos_imp(T r_t, bool erase_entry_if_last, size_type pos)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    entry_pointer p_e = m_a_p_entries[pos];

  my_resize_base::notify_erase_search_start();

  if (p_e == NULL)
    {
      my_resize_base::notify_erase_search_end();

      PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(
								    ext_eraser::extract_key(r_t));)

	PB_ASSOC_DBG_ONLY(assert_valid();)

	return (0);
    }

  if (my_hash_eq_fn_base::operator()(
				     p_e->m_value.first,
				     my_traits_base::ext_eraser::extract_key(r_t)))
    {
      my_resize_base::notify_erase_search_end();

      PB_ASSOC_DBG_ONLY(my_map_debug_base::
			check_key_exists(my_traits_base::ext_eraser::extract_key(r_t));)

	std::pair<size_type, bool> ers_pair =
	my_traits_base::ext_eraser::erase(
					  PB_ASSOC_EP2VP(m_a_p_entries[pos]),
					  r_t, erase_entry_if_last);

      if (ers_pair.second)
	{
	  erase_entry_pointer(m_a_p_entries[pos]);

	  do_resize_if_needed_no_throw();
	}

      PB_ASSOC_DBG_ONLY(assert_valid();)

	return (ers_pair.first);
    }

  while (true)
    {
      entry_pointer p_next_e = p_e->m_p_next;

      if (p_next_e == NULL)
	{
	  my_resize_base::notify_erase_search_end();

	  PB_ASSOC_DBG_ONLY(my_map_debug_base::
			    check_key_does_not_exist(
						     my_traits_base::ext_eraser::extract_key(r_t));)

	    PB_ASSOC_DBG_ONLY(assert_valid();)

	    return (0);
	}

      if (my_hash_eq_fn_base::operator()(
					 p_next_e->m_value.first,
					 my_traits_base::ext_eraser::extract_key(r_t)))
	{
	  my_resize_base::notify_erase_search_end();

	  PB_ASSOC_DBG_ONLY(my_map_debug_base::
			    check_key_exists(
					     my_traits_base::ext_eraser::extract_key(r_t));)

	    std::pair<size_type, bool> ers_pair =
	    my_traits_base::ext_eraser::erase(
					      PB_ASSOC_EP2VP(p_e->m_p_next),
					      r_t, erase_entry_if_last);

	  if (ers_pair.second)
	    {
	      erase_entry_pointer(p_e->m_p_next);

	      do_resize_if_needed_no_throw();
	    }

	  PB_ASSOC_DBG_ONLY(assert_valid();)

	    return (ers_pair.first);
	}

      my_resize_base::notify_erase_search_collision();

      p_e = p_next_e;
    }
}

