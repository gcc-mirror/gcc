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
 * @file constructor_destructor_fn_imps.hpp
 * Contains implementations of PB_ASSOC_CLASS_NAME.
 */

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::entry_allocator
PB_ASSOC_CLASS_C_DEC::s_entry_allocator;

PB_ASSOC_CLASS_T_DEC
template<class It>
void
PB_ASSOC_CLASS_C_DEC::
copy_from_range(It first_it, It last_it)
{
  while (first_it != last_it)
    insert(*(first_it++));
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME() :
  m_p_l(s_entry_allocator.allocate(1)),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Eq_Fn& r_eq_fn) :
  Eq_Fn(r_eq_fn),
  m_p_l(s_entry_allocator.allocate(1)),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const Eq_Fn& r_eq_fn, const Update_Policy& r_update_policy) :
  Eq_Fn(r_eq_fn),
  Update_Policy(r_update_policy),
  m_p_l(s_entry_allocator.allocate(1)),
  m_size(0)
{
  initialize();

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
PB_ASSOC_CLASS_NAME(const PB_ASSOC_CLASS_C_DEC& r_other) :
  Eq_Fn(r_other),
  Update_Policy(r_other),
  m_p_l(s_entry_allocator.allocate(1)),
  m_size(0)
{
  initialize();

  try
    {
      copy_from_range(r_other.begin(), r_other.end());
    }
  catch(...)
    {
      deallocate_all(true);

      throw;
    }

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

    std::swap(m_p_l, r_other.m_p_l);

  std::swap(m_size, r_other.m_size);

  std::swap((Eq_Fn& )(*this), (Eq_Fn& )r_other);

  Update_Policy::swap(r_other);

  PB_ASSOC_DBG_ONLY(my_map_debug_base::swap(r_other);)

    PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
deallocate_all(bool deallocate_root)
{
  entry_pointer p_l = m_p_l->m_p_next;

  while (p_l != NULL)
    {
      entry_pointer p_next_l = p_l->m_p_next;

      p_l->~entry();

      s_entry_allocator.deallocate(p_l, 1);

      p_l = p_next_l;
    }

  if (deallocate_root)
    s_entry_allocator.deallocate(m_p_l, 1);
  else
    m_p_l->m_p_next = NULL;
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~PB_ASSOC_CLASS_NAME()
{
  deallocate_all(true);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
initialize()
{
  m_p_l->m_p_next = NULL;
}
