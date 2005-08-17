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

/*
 * @file hash_load_check_resize_trigger_imp.hpp.hpp
 * Contains an implementation of hash_load_check_resize_trigger..
 */

PB_ASSOC_CLASS_T_DEC
pb_assoc::detail::int_to_type<External_Load_Access>
PB_ASSOC_CLASS_C_DEC::s_external_load_access_ind;

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
hash_load_check_resize_trigger(float load_min, float load_max) :
  m_load_min(load_min),
  m_load_max(load_max),
  m_next_shrink_size(0),
  m_next_grow_size(0),
  m_resize_needed(false)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_start()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_collision()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_end()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_start()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_collision()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_end()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_start()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_collision()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_end()
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_inserted(size_type num_entries)
{
  PB_ASSOC_DBG_ASSERT(num_entries <= m_next_grow_size);

  my_size_base::set_size(num_entries);

  m_resize_needed = (num_entries == m_next_grow_size);

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erased(size_type num_entries)
{
  PB_ASSOC_DBG_ASSERT(num_entries >= m_next_shrink_size);

  my_size_base::set_size(num_entries);

  m_resize_needed = (num_entries == m_next_shrink_size);

  PB_ASSOC_DBG_ONLY(assert_valid();)

    PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
is_resize_needed() const
{
  PB_ASSOC_DBG_ONLY(assert_valid();)

    return (m_resize_needed);
}

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
is_grow_needed(size_type /*size*/, size_type num_entries) const
{
  PB_ASSOC_DBG_ASSERT(m_resize_needed);

  return (num_entries >= m_next_grow_size);
}

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
is_shrink_needed(size_type /*size*/, size_type num_entries) const
{
  PB_ASSOC_DBG_ASSERT(m_resize_needed);

  return (num_entries <= m_next_shrink_size);
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~hash_load_check_resize_trigger()
{ }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
notify_resized(size_type new_size)
{
  m_resize_needed = false;

  m_next_grow_size =
    size_type(m_load_max*  new_size - 1);

  m_next_shrink_size =
    size_type(m_load_min*  new_size );

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
notify_externally_resized(size_type new_size)
{
  m_resize_needed = false;

  size_type new_grow_size =
    size_type(m_load_max*  new_size - 1);

  size_type new_shrink_size =
    size_type(m_load_min*  new_size );

  if (new_grow_size >= m_next_grow_size)
    {
      PB_ASSOC_DBG_ASSERT(new_shrink_size > m_next_shrink_size);

      m_next_grow_size = new_grow_size;

      PB_ASSOC_DBG_ONLY(assert_valid();)

	return;
    }

  PB_ASSOC_DBG_ASSERT(new_shrink_size <= m_next_shrink_size);

  m_next_shrink_size = new_shrink_size;

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
notify_cleared()
{
  my_size_base::set_size(0);

  m_resize_needed = (0 < m_next_shrink_size);

  PB_ASSOC_DBG_ONLY(assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)

    my_size_base::swap(r_other);

  std::swap(m_load_min, r_other.m_load_min);
  std::swap(m_load_max, r_other.m_load_max);

  std::swap(m_resize_needed, r_other.m_resize_needed);

  std::swap(m_next_grow_size, r_other.m_next_grow_size);
  std::swap(m_next_shrink_size, r_other.m_next_shrink_size);

  PB_ASSOC_DBG_ONLY(assert_valid();)
    PB_ASSOC_DBG_ONLY(r_other.assert_valid();)
    }

PB_ASSOC_CLASS_T_DEC
inline std::pair<float, float>
PB_ASSOC_CLASS_C_DEC::
get_loads() const
{
  return (get_loads_imp(s_external_load_access_ind));
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
set_loads(std::pair<float, float> load_pair)
{
  set_loads_imp(load_pair, s_external_load_access_ind);
}

PB_ASSOC_CLASS_T_DEC
inline std::pair<float, float>
PB_ASSOC_CLASS_C_DEC::
get_loads_imp(pb_assoc::detail::int_to_type<true>) const
{
  return (std::make_pair(m_load_min, m_load_max));
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
set_loads_imp(std::pair<float, float> load_pair, pb_assoc::detail::int_to_type<true>)
{
  const float old_load_min = m_load_min;
  const float old_load_max = m_load_max;
  const size_type old_next_shrink_size = m_next_shrink_size;
  const size_type old_next_grow_size = m_next_grow_size;
  const bool old_resize_needed = m_resize_needed;

  try
    {
      m_load_min = load_pair.first;
      m_load_max = load_pair.second;

      do_resize(static_cast<size_type>(
				       my_size_base::get_size() / ((m_load_min + m_load_max) / 2)));
    }
  catch(...)
    {
      m_load_min = old_load_min;
      m_load_max = old_load_max;
      m_next_shrink_size = old_next_shrink_size;
      m_next_grow_size = old_next_grow_size;
      m_resize_needed = old_resize_needed;

      throw;
    }
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
do_resize(size_type /*new_size*/)
{
  abort();
}

#ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG
PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_valid() const
{
  PB_ASSOC_DBG_ASSERT(m_load_max > m_load_min);

  PB_ASSOC_DBG_ASSERT(m_next_grow_size >= m_next_shrink_size);
}
#endif // #ifdef PB_ASSOC_HT_LOAD_CHECK_RESIZE_TRIGGER_DEBUG
