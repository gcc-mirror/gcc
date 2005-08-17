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
 * @file find_no_store_hash_fn_imps.hpp
 * Contains implementations of gp_ht_map_'s find related functions, when the hash
 *	value is not stored.
 */

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_data_reference
PB_ASSOC_CLASS_C_DEC::
const_subscript_imp(const_key_reference r_key, int_to_type<false>) const
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).
	    find_key_pointer(r_key, my_traits_base::m_store_hash_indicator)->second);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::pointer
PB_ASSOC_CLASS_C_DEC::find_key_pointer(const_key_reference r_key, int_to_type<false>)
{
  const size_type hash = my_ranged_probe_fn_base::operator()(r_key);

  size_type i;

  my_resize_base::notify_find_search_start();

  // Loop until entry is found or until all possible entries accessed.

  for (i = 0; i < m_num_e; ++i)
    {
      const size_type pos =
	my_ranged_probe_fn_base::operator()(r_key, hash, i);

      entry* const p_e = m_a_entries + pos;

      switch(p_e->m_stat)
	{
	case EMPTY_ENTRY_STATUS:
	  {
	    my_resize_base::notify_find_search_end();

	    PB_ASSOC_DBG_ONLY(my_map_debug_base::
			      check_key_does_not_exist(r_key);)

	      return (NULL);
	  }
	  break;
	case VALID_ENTRY_STATUS:
	  if (my_hash_eq_fn_base::operator()(
					     PB_ASSOC_V2F(p_e->m_value),
					     r_key))
	    {
	      my_resize_base::notify_find_search_end();

	      PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_exists(r_key);)

		return ((pointer)&p_e->m_value);
	    }
	  break;
	case ERASED_ENTRY_STATUS:
	  break;
	default:
	  PB_ASSOC_DBG_ASSERT(0);
	};

      my_resize_base::notify_find_search_collision();
    }

  PB_ASSOC_DBG_ONLY(my_map_debug_base::
		    check_key_does_not_exist(r_key);)

    my_resize_base::notify_find_search_end();

  return (NULL);
}

