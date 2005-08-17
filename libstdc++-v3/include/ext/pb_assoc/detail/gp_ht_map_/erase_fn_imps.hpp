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
 * @file erase_fn_imps.hpp
 * Contains implementations of gp_ht_map_'s erase related functions.
 */

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
erase_entry(entry_pointer p_e)
{
  PB_ASSOC_DBG_ASSERT(p_e->m_stat = VALID_ENTRY_STATUS);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::erase_existing(p_e->m_value.first);)

    p_e->m_value.~value_type();

  p_e->m_stat = ERASED_ENTRY_STATUS;

  PB_ASSOC_DBG_ASSERT(m_num_used_e > 0);
  my_resize_base::notify_erased(--m_num_used_e);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
clear()
{
  for (size_type pos = 0; pos < m_num_e; ++pos)
    {
      entry_pointer p_e =& m_a_entries[pos];

      if (p_e->m_stat == VALID_ENTRY_STATUS)
	erase_entry(p_e);
    }

  do_resize_if_needed_no_throw();

  my_resize_base::notify_cleared();
}

PB_ASSOC_CLASS_T_DEC
template<class Pred>
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
erase_if(Pred pred)
{
  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

    size_type num_ersd = 0;

  for (size_type pos = 0; pos < m_num_e; ++pos)
    {
      entry_pointer p_e =& m_a_entries[pos];

      if (p_e->m_stat == VALID_ENTRY_STATUS&&  pred(p_e->m_value))
	{
	  ++num_ersd;

	  erase_entry(p_e);
	}
    }

  do_resize_if_needed_no_throw();

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

    return (num_ersd);
}

#include <ext/pb_assoc/detail/gp_ht_map_/erase_no_store_hash_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/erase_store_hash_fn_imps.hpp>
