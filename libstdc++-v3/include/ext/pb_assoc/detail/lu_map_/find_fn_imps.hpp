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
 * @file find_fn_imps.hpp
 * Contains implementations of lu_map_.
 */

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::find_iterator
PB_ASSOC_CLASS_C_DEC::
find_end()
{
  return (NULL);
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_find_iterator
PB_ASSOC_CLASS_C_DEC::
find_end() const
{
  return (NULL);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
move_next_to_front(entry_pointer p_l) const
{
  entry_pointer p_move_l = p_l->m_p_next;

  PB_ASSOC_DBG_ASSERT(p_move_l != NULL);

  p_l->m_p_next = p_move_l->m_p_next;

  p_move_l->m_p_next = m_p_l->m_p_next;

  m_p_l->m_p_next = p_move_l;
}

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_data_reference
PB_ASSOC_CLASS_C_DEC::
const_subscript_imp(const_key_reference r_key) const
{
  entry_pointer p_l = find_imp(r_key);

  PB_ASSOC_DBG_ASSERT(p_l->m_p_next != NULL)

    if (Update_Policy::operator()(p_l->m_update_metadata))
      {
	move_next_to_front(p_l);

	return (m_p_l->m_p_next->m_value.second);
      }
    else
      return (p_l->m_p_next->m_value.second);
}
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::find_iterator
PB_ASSOC_CLASS_C_DEC::find(const_key_reference r_key)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    entry_pointer p_l = find_imp(r_key);

  if (p_l->m_p_next != NULL)
    {
      PB_ASSOC_DBG_ONLY(assert_valid();)

	return (&p_l->m_p_next->m_value);
    }

  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (find_end());
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::const_find_iterator
PB_ASSOC_CLASS_C_DEC::find(const_key_reference r_key) const
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    entry_pointer p_l = find_imp(r_key);

  if (p_l->m_p_next != NULL)
    {
      PB_ASSOC_DBG_ONLY(assert_valid();)

	return (&m_p_l->m_p_next->m_value);
    }

  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (find_end());
}
