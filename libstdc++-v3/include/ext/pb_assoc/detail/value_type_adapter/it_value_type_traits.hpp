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
 * @file it_value_type_traits.hpp
 * Contains an adapter of mapping levels.
 */

template<class Base, bool Lowest>
struct base_it_key_type
{
  typedef typename Base::it_key_type type;
};

template<class Base>
struct base_it_key_type<
  Base,
  true>
{
  typedef typename Base::const_key_reference type;
};

template<typename Base_Key_,
	 typename Key_,
	 typename Data_,
	 typename Value_,
	 class Allocator_ >
struct it_value_type_traits_
{
  typedef ref_pair< Base_Key_, Key_> key_ref_pair;

  typedef
  typename Allocator_::template rebind<
    key_ref_pair>::other::const_reference
  key_ref_pair_val;

  typedef key_ref_pair_val key_type;

  typedef ref_pair< key_ref_pair_val, Data_> value_type;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::reference
  reference;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_reference
  const_reference;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::pointer
  pointer;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_pointer
  const_pointer;

  typedef
  typename Allocator_::template rebind<
    int* >::other::value_type
  buf_t;

  struct value_type_holder
  {
    buf_t m_a_key_buf[sizeof(key_ref_pair) / sizeof(buf_t) + 1];

    buf_t m_a_value_buf[sizeof(value_type) / sizeof(buf_t) + 1];
  };

  typedef
  typename Allocator_::template rebind<
    value_type_holder>::other::reference
  value_type_hoder_valerence;

  inline static pointer
  recast(value_type_hoder_valerence r_holder)
  {
    return reinterpret_cast<pointer>(r_holder.m_a_value_buf);
  }

  inline static void
  make_valid(value_type_hoder_valerence r_holder, Base_Key_ r_bk, Value_ r_val)
  {
    typedef
      typename Allocator_::template rebind<
      void* >::other::value_type
      void_pointer;

    void_pointer p_target = r_holder.m_a_key_buf;

    new (p_target) key_ref_pair(r_bk, r_val.first);

    typedef
      typename Allocator_::template rebind<
      key_ref_pair>::other::pointer
      key_ref_pair_pointer;

    key_ref_pair_pointer p_key =
      reinterpret_cast<key_ref_pair_pointer>(r_holder.m_a_key_buf);

    p_target = r_holder.m_a_value_buf;

    new (p_target) value_type(*p_key, r_val.second);
  }
};

template<typename Base_Key_,
	 typename Key_,
	 typename Value_,
	 class Allocator_>
struct it_value_type_traits_<
  Base_Key_,
  Key_,
  null_data_type,
  Value_,
  Allocator_>
{
  typedef ref_pair< Base_Key_, Key_> key_ref_pair;

  typedef
  typename Allocator_::template rebind<
    key_ref_pair>::other::const_reference
  key_ref_pair_val;

  typedef key_ref_pair_val key_type;

  typedef key_ref_pair value_type;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_reference
  reference;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_reference
  const_reference;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_pointer
  pointer;

  typedef
  typename Allocator_::template rebind<
    value_type>::other::const_pointer
  const_pointer;

  typedef
  typename Allocator_::template rebind<
    int* >::other::value_type
  buf_t;

  struct value_type_holder
  {
    buf_t m_a_key_buf[sizeof(key_ref_pair) / sizeof(buf_t) + 1];

    buf_t m_a_value_buf[sizeof(value_type) / sizeof(buf_t) + 1];
  };

  typedef
  typename Allocator_::template rebind<
    value_type_holder>::other::reference
  value_type_hoder_valerence;

  inline static pointer
  recast(value_type_hoder_valerence r_holder)
  {
    return reinterpret_cast<pointer>(r_holder.m_a_value_buf);
  }

  inline static void
  make_valid(value_type_hoder_valerence r_holder, Base_Key_ r_bk, Value_ r_val)
  {
    typedef
      typename Allocator_::template rebind<
      void* >::other::value_type
      void_pointer;

    void_pointer p_target = r_holder.m_a_value_buf;

    new (p_target) key_ref_pair(r_bk, r_val.first);
  }
};

