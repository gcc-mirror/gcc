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
 * @file resize_fn_imps.hpp
 * Contains implementations of gp_ht_map_'s resize related functions.
 */

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
do_resize_if_needed()
{
  if (!my_resize_base::is_resize_needed())
    return (false);

  do
    do_resize(my_resize_base::get_new_size(m_num_e, m_num_used_e));
  while (my_resize_base::is_resize_needed());

  return (true);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
do_resize_if_needed_no_throw()
{
  if (!my_resize_base::is_resize_needed())
    return;

  try
    {
      do
	do_resize(my_resize_base::get_new_size(m_num_e, m_num_used_e));
      while (my_resize_base::is_resize_needed());
    }
  catch(...)
    { }

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
do_resize(size_type new_size)
{
#ifdef PB_ASSOC_BASIC_REGRESSION
  throw_prob_adjustor adjust(m_num_e);
#endif // #ifdef PB_ASSOC_BASIC_REGRESSION

  PB_ASSOC_DBG_ONLY(assert_valid();)

    const size_type old_size = m_num_e;

  entry_array a_entries_resized = NULL;

  my_ranged_probe_fn_base::notify_resized(new_size);

  // Following line might throw an exception.

  try
    {
      a_entries_resized = s_entry_allocator.allocate(new_size);
    }
  catch(...)
    {
      my_ranged_probe_fn_base::notify_resized(old_size);

      throw;
    }

  m_num_e = new_size;

  for (size_type i = 0; i < m_num_e; ++i)
    a_entries_resized[i].m_stat = (entry_status)EMPTY_ENTRY_STATUS;

  try
    {
      resize_imp(a_entries_resized, old_size);
    }
  catch(...)
    {
      erase_all_valid_entries(a_entries_resized, new_size);

      m_num_e = old_size;

      s_entry_allocator.deallocate(a_entries_resized, new_size);

      my_ranged_probe_fn_base::notify_resized(old_size);

      throw;
    }

  // At this point no exceptions can be thrown.

  PB_ASSOC_DBG_ONLY(assert_entry_array_valid(a_entries_resized, my_hash_traits_base::s_store_hash_indicator);)

    Resize_Policy::notify_resized(new_size);

  erase_all_valid_entries(m_a_entries, old_size);

  s_entry_allocator.deallocate(m_a_entries, old_size);

  m_a_entries = a_entries_resized;

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
resize_imp(entry_array a_entries_resized, size_type old_size)
{
  for (size_type pos = 0; pos < old_size; ++pos)
    if (m_a_entries[pos].m_stat == VALID_ENTRY_STATUS)
      resize_imp_reassign(m_a_entries + pos, a_entries_resized, my_hash_traits_base::s_store_hash_indicator);
}

#include <ext/pb_assoc/detail/gp_ht_map_/resize_no_store_hash_fn_imps.hpp>
#include <ext/pb_assoc/detail/gp_ht_map_/resize_store_hash_fn_imps.hpp>
