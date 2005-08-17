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
 * @file hash_standard_resize_policy_imp.hpp
 * Contains an implementation of hash_standard_resize_policy.
 */

#ifdef PB_ASSOC_HT_STANDARD_RESIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HT_STANDARD_RESIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HT_STANDARD_RESIZE_POLICY_DEBUG

PB_ASSOC_CLASS_T_DEC
pb_assoc::detail::int_to_type<External_Size_Access>
PB_ASSOC_CLASS_C_DEC::s_external_size_access_indicator;

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
hash_standard_resize_policy(size_type suggested_size /*= 8*/) :
  m_size(Size_Policy::get_init_size(suggested_size))
{
  my_trigger_policy_base::notify_externally_resized(
						    Size_Policy::get_init_size(suggested_size));
}

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
hash_standard_resize_policy(const Size_Policy& r_size_policy, size_type suggested_size /*= 8*/) :
  Size_Policy(r_size_policy),
  m_size(Size_Policy::get_init_size(suggested_size))
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
hash_standard_resize_policy(const Size_Policy& r_size_policy, const Trigger_Policy& r_trigger_policy, size_type suggested_size /*= 8*/) :
  Size_Policy(r_size_policy),
  Trigger_Policy(r_trigger_policy),
  m_size(Size_Policy::get_init_size(suggested_size))
{ }

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
~hash_standard_resize_policy()
{ }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  my_trigger_policy_base::swap(r_other);

  my_size_policy_base::swap(r_other);

  std::swap(m_size, r_other.m_size);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_start()
{
  my_trigger_policy_base::notify_find_search_start();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_collision()
{
  my_trigger_policy_base::notify_find_search_collision();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_find_search_end()
{
  my_trigger_policy_base::notify_find_search_end();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_start()
{
  my_trigger_policy_base::notify_insert_search_start();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_collision()
{
  my_trigger_policy_base::notify_insert_search_collision();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_insert_search_end()
{
  my_trigger_policy_base::notify_insert_search_end();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_start()
{
  my_trigger_policy_base::notify_erase_search_start();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_collision()
{
  my_trigger_policy_base::notify_erase_search_collision();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erase_search_end()
{
  my_trigger_policy_base::notify_erase_search_end();
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_inserted(size_type num_e)
{
  my_trigger_policy_base::notify_inserted(num_e);
}

PB_ASSOC_CLASS_T_DEC
inline void
PB_ASSOC_CLASS_C_DEC::
notify_erased(size_type num_e)
{
  my_trigger_policy_base::notify_inserted(num_e);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
notify_cleared()
{
  my_trigger_policy_base::notify_cleared();
}

PB_ASSOC_CLASS_T_DEC
inline bool
PB_ASSOC_CLASS_C_DEC::
is_resize_needed() const
{
  return (my_trigger_policy_base::is_resize_needed());
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_new_size(size_type size, size_type num_used_e) const
{
  PB_ASSOC_DBG_ASSERT(m_size == size);

  if (my_trigger_policy_base::
      is_grow_needed(size, num_used_e))
    return (my_size_policy_base::get_nearest_larger_size(m_size));

  PB_ASSOC_DBG_ASSERT(my_trigger_policy_base::
		      is_shrink_needed(size, num_used_e));

  return (my_size_policy_base::get_nearest_smaller_size(m_size));
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_init_size() const
{
  PB_ASSOC_DBG_ASSERT(m_size ==
		      my_trigger_policy_base::get_init_size());

  return (m_size);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
notify_resized(size_type new_size)
{
  my_trigger_policy_base::notify_resized(new_size);

  m_size = new_size;
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_actual_size() const
{
  return (get_actual_size(s_external_size_access_indicator));
}

PB_ASSOC_CLASS_T_DEC
inline typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_actual_size(external_resize_true_indicator) const
{
  return (m_size);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
resize(size_type new_size)
{
  resize(new_size, s_external_size_access_indicator);
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
resize(size_type new_size, external_resize_true_indicator)
{
  size_type actual_new_size = my_size_policy_base::get_init_size(1);
  while (actual_new_size < new_size)
    {
      const size_type pot =
	my_size_policy_base::get_nearest_larger_size(actual_new_size);

      if (pot == actual_new_size&&  pot < new_size)
	throw cannot_resize();

      actual_new_size = pot;
    }

  const size_type old_size = m_size;

  try
    {
      do_resize(actual_new_size);
    }
  catch(cannot_insert& )
    {
      m_size = old_size;

      throw cannot_resize();
    }
  catch(...)
    {
      m_size = old_size;

      throw;
    }
}

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
do_resize(size_type /*new_size*/)
{
  // Tmp Ami
  abort();
}

PB_ASSOC_CLASS_T_DEC
Trigger_Policy& 
PB_ASSOC_CLASS_C_DEC::
get_trigger_policy()
{
  return (*this);
}

PB_ASSOC_CLASS_T_DEC
const Trigger_Policy& 
PB_ASSOC_CLASS_C_DEC::
get_trigger_policy() const
{
  return (*this);
}

PB_ASSOC_CLASS_T_DEC
Size_Policy& 
PB_ASSOC_CLASS_C_DEC::
get_size_policy()
{
  return (*this);
}

PB_ASSOC_CLASS_T_DEC
const Size_Policy& 
PB_ASSOC_CLASS_C_DEC::
get_size_policy() const
{
  return (*this);
}
