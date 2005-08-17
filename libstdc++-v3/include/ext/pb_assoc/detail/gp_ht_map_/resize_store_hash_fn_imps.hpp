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
 * @file resize_store_hash_fn_imps.hpp
 * Contains implementations of gp_ht_map_'s resize related functions, when the
 *	hash value is stored.
 */

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
resize_imp_reassign(entry_pointer p_e, entry_array a_entries_resized, int_to_type<true>)
{
  const_key_reference r_key = PB_ASSOC_V2F(p_e->m_value);

  size_type hash =
    my_ranged_probe_fn_base::operator()(r_key, p_e->m_hash);

  size_type i;

  for (i = 0; i < m_num_e; ++i)
    {
      const size_type pos =
	my_ranged_probe_fn_base::operator()(r_key, hash, i);

      entry* const p_new_e = a_entries_resized + pos;

      switch(p_new_e->m_stat)
	{
	case EMPTY_ENTRY_STATUS:
#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
	  new (&p_new_e->m_value) mapped_value_type(p_e->m_value);
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

#ifdef PB_ASSOC_DATA_FALSE_INDICATOR
	  new (&p_new_e->m_value) Key(p_e->m_value);
#endif // #ifdef PB_ASSOC_DATA_FALSE_INDICATO

	  p_new_e->m_hash = hash;

	  p_new_e->m_stat = VALID_ENTRY_STATUS;

	  return;
	case ERASED_ENTRY_STATUS:
	  PB_ASSOC_DBG_ASSERT(0);
	  break;
	case VALID_ENTRY_STATUS:
	  break;
	default:
	  PB_ASSOC_DBG_ASSERT(0);
	};
    }

  throw cannot_insert();
}

