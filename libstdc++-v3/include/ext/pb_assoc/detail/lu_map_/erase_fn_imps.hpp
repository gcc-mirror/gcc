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
 * Contains implementations of lu_map_.
 */

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
erase(const_key_reference r_key)
{
  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

    entry_pointer p_l = find_imp(r_key);

  if (p_l->m_p_next == NULL)
    return (0);

  erase_imp(p_l);

  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (1);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
clear()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    PB_ASSOC_DBG_ONLY(my_map_debug_base::clear();)

    deallocate_all(false);

  m_size = 0;

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
template<class Pred>
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
erase_if(Pred& r_pred)
{
  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

    size_type num_ersd = 0;

  entry_pointer p_l = m_p_l;

  while (p_l->m_p_next != NULL)
    {
      if (r_pred(p_l->m_p_next->m_value))
	{
	  erase_imp(p_l);

	  ++num_ersd;
	}
      else
	p_l = p_l->m_p_next;
    }

  PB_ASSOC_DBG_ONLY(PB_ASSOC_CLASS_C_DEC::assert_valid();)

    return (num_ersd);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
erase_imp(entry_pointer p_l)
{
  PB_ASSOC_DBG_ASSERT(p_l->m_p_next != NULL);

  entry_pointer p_next_l = p_l->m_p_next->m_p_next;

  PB_ASSOC_DBG_ONLY(my_map_debug_base::erase_existing(
						      PB_ASSOC_V2F(p_l->m_p_next->m_value));)

    p_l->m_p_next->m_value.~mapped_value_type();

  s_entry_allocator.deallocate(p_l->m_p_next, 1);

  p_l->m_p_next = p_next_l;

  PB_ASSOC_DBG_ASSERT(m_size > 0);
  --m_size;
}
