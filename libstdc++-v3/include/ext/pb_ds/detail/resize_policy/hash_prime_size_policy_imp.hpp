// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file hash_prime_size_policy_imp.hpp
 * Contains a resize size policy implementation.
 */

namespace detail
{

  enum
    {
      num_distinct_sizes = 31
    };

  // Taken from the SGI implementation; acknowledged in the docs.

  static const std::size_t g_a_sizes[num_distinct_sizes] =
    {
      /* Dealing cards... */
      /* 0    */ 5ul,
      /* 1    */ 11ul,
      /* 2    */ 23ul,
      /* 3    */ 53ul,
      /* 4    */ 97ul,
      /* 5    */ 193ul,
      /* 6    */ 389ul,
      /* 7    */ 769ul,
      /* 8    */ 1543ul,
      /* 9    */ 3079ul,
      /* 10    */ 6151ul,
      /* 11     */ 12289ul,
      /* 12     */ 24593ul,
      /* 13    */ 49157ul,
      /* 14    */ 98317ul,
      /* 15    */ 196613ul,
      /* 16    */ 393241ul,
      /* 17    */ 786433ul,
      /* 18    */ 1572869ul,
      /* 19    */ 3145739ul,
      /* 20    */ 6291469ul,
      /* 21    */ 12582917ul,
      /* 22    */ 25165843ul,
      /* 23    */ 50331653ul,
      /* 24    */ 100663319ul,
      /* 25    */ 201326611ul,
      /* 26    */ 402653189ul,
      /* 27    */ 805306457ul,
      /* 28    */ 1610612741,
      /* 29    */ 3221225473ul,
      /* 30    */ 4294967291ul
      /* Pot's good, let's play */
    };

} // namespace detail

PB_DS_CLASS_T_DEC
inline
PB_DS_CLASS_C_DEC::
hash_prime_size_policy(size_type start_size) :
  m_start_size(start_size)
{
  m_start_size =
    get_nearest_larger_size(start_size);
}

PB_DS_CLASS_T_DEC
inline void
PB_DS_CLASS_C_DEC::
swap(PB_DS_CLASS_C_DEC& other)
{
  std::swap(m_start_size, other.m_start_size);
}

PB_DS_CLASS_T_DEC
inline PB_DS_CLASS_C_DEC::size_type
PB_DS_CLASS_C_DEC::
get_nearest_larger_size(size_type size) const
{
  const std::size_t* const p_upper =
    std::upper_bound(            detail::g_a_sizes, detail::g_a_sizes + detail::num_distinct_sizes, size);

  if (p_upper == detail::g_a_sizes + detail::num_distinct_sizes)
    throw resize_error();

  return (*p_upper);
}

PB_DS_CLASS_T_DEC
inline PB_DS_CLASS_C_DEC::size_type
PB_DS_CLASS_C_DEC::
get_nearest_smaller_size(size_type size) const
{
  const size_t* p_lower = std::lower_bound(        detail::g_a_sizes, detail::g_a_sizes + detail::num_distinct_sizes, size);

  if (*p_lower >= size&&  p_lower != detail::g_a_sizes)
    --p_lower;

  if (*p_lower < m_start_size)
    return (m_start_size);

  return (*p_lower);
}
