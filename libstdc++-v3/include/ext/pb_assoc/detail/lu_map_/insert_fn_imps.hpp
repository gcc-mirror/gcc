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
 * @file insert_fn_imps.hpp
 * Contains implementations of lu_map_.
 */

PB_ASSOC_CLASS_T_DEC
inline std::pair<
  typename PB_ASSOC_CLASS_C_DEC::find_iterator,
  bool>
PB_ASSOC_CLASS_C_DEC::
insert(const_reference r_val)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    entry_pointer p_l = find_imp(PB_ASSOC_V2F(r_val));

  bool found = true;

  if (p_l->m_p_next == NULL)
    {
      found = false;

      insert_new_after(p_l, r_val);
    }

  PB_ASSOC_DBG_ASSERT(Eq_Fn::operator()(
					PB_ASSOC_V2F(p_l->m_p_next->m_value),
					PB_ASSOC_V2F(r_val)));

  PB_ASSOC_DBG_ONLY(assert_valid();)

    if (Update_Policy::operator()(p_l->m_update_metadata))
      {
	move_next_to_front(p_l);

	return (std::make_pair(
			       find_iterator(&m_p_l->m_p_next->m_value),
			       !found));
      }
    else
      return (std::make_pair(
			     find_iterator(&p_l->m_p_next->m_value),
			     !found));
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
insert_new_after(entry_pointer p_l, const_reference r_val)
{
  PB_ASSOC_DBG_ONLY(my_map_debug_base::check_key_does_not_exist(
								PB_ASSOC_V2F(r_val));)

    entry_pointer p_new_l = s_entry_allocator.allocate(1);

  cond_dealtor_t cond(p_new_l);

  new (const_cast<void* >(
			  static_cast<const void* >(&p_new_l->m_value)))
    value_type(r_val);

  cond.set_no_action();

  PB_ASSOC_DBG_ASSERT(p_l->m_p_next == NULL);
  p_l->m_p_next = p_new_l;

  p_new_l->m_p_next = NULL;

  ++m_size;

  PB_ASSOC_DBG_ONLY(my_map_debug_base::insert_new(PB_ASSOC_V2F(r_val));)
    }

#ifdef PB_ASSOC_DATA_TRUE_INDICATOR
PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::data_reference
PB_ASSOC_CLASS_C_DEC::
subscript_imp(const_key_reference r_key)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    entry_pointer p_l = find_imp(r_key);

  bool found = true;

  if (p_l->m_p_next == NULL)
    {
      found = false;

      insert_new_after(p_l, value_type(r_key, data_type()));
    }

  PB_ASSOC_DBG_ASSERT(Eq_Fn::operator()(
					PB_ASSOC_V2F(p_l->m_p_next->m_value),
					PB_ASSOC_V2F(r_val)));

  PB_ASSOC_DBG_ONLY(assert_valid();)

    if (Update_Policy::operator()(p_l->m_update_metadata))
      {
	move_next_to_front(p_l);

	return (m_p_l->m_p_next->m_value.second);
      }
    else
      return (p_l->m_p_next->m_value.second);
}
#endif // #ifdef PB_ASSOC_DATA_TRUE_INDICATOR

