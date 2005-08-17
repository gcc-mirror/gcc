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
 * @file find_store_hash_fn_imps.hpp
 * Contains implementations of cc_ht_map_'s find related functions, when the hash
 *	value is stored.
 */

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_data_reference
PB_ASSOC_CLASS_C_DEC::
const_subscript_imp(const_key_reference r_key, int_to_type<true>)  const
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (const_cast<PB_ASSOC_CLASS_C_DEC& >(*this).find_key_pointer(r_key, my_traits_base::m_store_hash_indicator)->second);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::pointer
PB_ASSOC_CLASS_C_DEC::
find_key_pointer(const_key_reference r_key, int_to_type<true>)
{
  comp_hash pos_hash_pair = my_ranged_hash_fn_base::operator()(r_key);

  entry_pointer p_e = m_a_p_entries[pos_hash_pair.first];

  my_resize_base::notify_find_search_start();

  while (p_e != NULL&& 
	 !my_hash_eq_fn_base::operator()(PB_ASSOC_V2F(p_e->m_value),
					 p_e->m_hash,
					 r_key,
					 pos_hash_pair.second))
    {
      my_resize_base::notify_find_search_collision();

      p_e = p_e->m_p_next;
    }

  my_resize_base::notify_find_search_end();

#ifdef PB_ASSOC_CC_HT_MAP_DEBUG
  if (p_e == NULL)
    my_map_debug_base::check_key_does_not_exist(r_key);
  else
    my_map_debug_base::check_key_exists(r_key);
#endif // #ifdef PB_ASSOC_CC_HT_MAP_DEBUG

  return (const_cast<pointer>(&p_e->m_value));
}
