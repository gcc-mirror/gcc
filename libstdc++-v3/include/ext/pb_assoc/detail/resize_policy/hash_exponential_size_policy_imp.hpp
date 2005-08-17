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
 * @file hash_exponential_size_policy_imp.hpp
 * Contains an implementation of hash_exponential_size_policy.
 */

#ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X) assert(X)
#define PB_ASSOC_DBG_VERIFY(X) assert(X)
#define PB_ASSOC_DBG_ONLY(X) X
#else // #ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG
#define PB_ASSOC_DBG_ASSERT(X)
#define PB_ASSOC_DBG_VERIFY(X) {if((X)==0);}
#define PB_ASSOC_DBG_ONLY(X) ;
#endif // #ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG

PB_ASSOC_CLASS_T_DEC
PB_ASSOC_CLASS_C_DEC::
hash_exponential_size_policy(size_type start_size, size_type grow_factor) :
  m_start_size(start_size),
  m_grow_factor(grow_factor)
{ }

PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
swap(PB_ASSOC_CLASS_C_DEC& r_other)
{
  std::swap(m_start_size, r_other.m_start_size);

  std::swap(m_grow_factor, r_other.m_grow_factor);
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_init_size(size_type suggested_size) const
{
  size_type ret = m_start_size;

  while (ret < suggested_size)
    ret *=  m_grow_factor;

  return (ret);
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_nearest_larger_size(size_type cur_size) const
{
  PB_ASSOC_DBG_ONLY(assert_is_one_of_my_sizes(size);)

    const size_type pot_ret = cur_size*  m_grow_factor;

  return ((pot_ret > cur_size)? pot_ret : cur_size);
}

PB_ASSOC_CLASS_T_DEC
typename PB_ASSOC_CLASS_C_DEC::size_type
PB_ASSOC_CLASS_C_DEC::
get_nearest_smaller_size(size_type cur_size) const
{
  PB_ASSOC_DBG_ONLY(assert_is_one_of_my_sizes(size);)

    const size_type pot_ret = cur_size / m_grow_factor;

  return ((pot_ret > 0)? pot_ret : m_start_size);
}

#ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG
PB_ASSOC_CLASS_T_DEC
void
PB_ASSOC_CLASS_C_DEC::
assert_is_one_of_my_sizes(size_type size)
{
  PB_ASSOC_DBG_ASSERT(size >= m_start_size);

  while (size >m_start_size)
    size /= m_grow_factor;

  PB_ASSOC_DBG_ASSERT(size == m_start_size);
}
#endif // #ifdef PB_ASSOC_HT_EXPONENTIAL_SIZE_POLICY_DEBUG

