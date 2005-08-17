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
 * @file types_traits.hpp
 * Contains a traits class of types used by containers.
 */

#ifndef TYPES_TRAITS_HPP
#define TYPES_TRAITS_HPP

#include <ext/pb_assoc/data_type.hpp>
#include <ext/pb_assoc/detail/type_utils.hpp>
#include <utility>

namespace pb_assoc
{

  namespace detail
  {

    template<typename Data, class Allocator>
    struct basic_data_types_traits
    {

    public:

      typedef
      typename Allocator::template rebind<
	Data>::other::value_type
      data_type;

      typedef
      typename Allocator::template rebind<
	Data>::other::pointer
      data_pointer;

      typedef
      typename Allocator::template rebind<
	Data>::other::const_pointer
      const_data_pointer;

      typedef
      typename Allocator::template rebind<
	Data>::other::reference
      data_reference;

      typedef
      typename Allocator::template rebind<
	Data>::other::const_reference
      const_data_reference;

    };

    template<typename Data, class Allocator>
    struct data_types_traits : public basic_data_types_traits<
      Data,
      Allocator>
    {

    public:

      typedef Data given_data_type;

    };

#define PB_ASSOC_CLASS_T_DEC \
	template<class Allocator>

#define PB_ASSOC_CLASS_C_DEC \
	data_types_traits< \
			null_data_type, \
			Allocator>

    template<class Allocator>
    struct data_types_traits<
      null_data_type,
      Allocator> : public basic_data_types_traits<
      null_data_type,
      Allocator>
    {

    public:

      typedef null_data_type given_data_type;

    };

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

    template<class Cntnr, class Allocator>
    struct data_types_traits<
      compound_data_type<
      Cntnr>,
      Allocator> : public basic_data_types_traits<
      Cntnr,
      Allocator>
    {

    public:

      typedef compound_data_type< Cntnr> given_data_type;

    };

#define PB_ASSOC_CLASS_T_DEC \
	template<typename Key, typename Data>

#define PB_ASSOC_CLASS_C_DEC \
	exception_throw_types_traits< \
		Key, \
		Data>

    template<typename Key, typename Data>
    struct exception_throw_types_traits
    {

    public:

      typedef int_to_type<false> no_throw_copies_false_indicator;

      typedef int_to_type<true> no_throw_copies_true_indicator;

    private:
      enum
	{
	  key_no_throw = is_simple_type<Key>::value,
	  data_no_throw = is_same_type<Data, null_data_type>::value ||
	  is_simple_type<Data>::value,
	  no_throw_copies = key_no_throw&&  data_no_throw
	};

      typedef int_to_type<no_throw_copies> no_throw_copies_t;

    public:
      static no_throw_copies_t s_no_throw_copies_indicator;
    };

    PB_ASSOC_CLASS_T_DEC
    typename PB_ASSOC_CLASS_C_DEC::no_throw_copies_t
    PB_ASSOC_CLASS_C_DEC::s_no_throw_copies_indicator;

#undef PB_ASSOC_CLASS_T_DEC

#undef PB_ASSOC_CLASS_C_DEC

    template<typename Key, typename Data, class Allocator>
    struct value_types_traits
    {

    public:

      typedef
      typename Allocator::template rebind<
	std::pair<const Key, Data> >::other
      value_type_allocator;

      typedef typename value_type_allocator::value_type value_type;

      typedef typename value_type_allocator::pointer pointer;

      typedef typename value_type_allocator::const_pointer const_pointer;

      typedef typename value_type_allocator::reference reference;

      typedef typename value_type_allocator::const_reference const_reference;

    };

    template<typename Key, class Allocator>
    struct value_types_traits<
      Key,
      null_data_type,
      Allocator>
    {

    public:

      typedef
      typename Allocator::template rebind<
	Key>::other
      value_type_allocator;

      typedef typename value_type_allocator::value_type value_type;

      typedef typename value_type_allocator::const_pointer pointer;

      typedef typename value_type_allocator::const_pointer const_pointer;

      typedef typename value_type_allocator::const_reference reference;

      typedef typename value_type_allocator::const_reference const_reference;

    };

    template<typename Key, class Cntnr, class Allocator>
    struct value_types_traits<
      Key,
      compound_data_type<
      Cntnr>,
      Allocator>
    {
    private:

      typedef
      typename Allocator::template rebind<
	std::pair<const Key, Cntnr> >::other
      value_type_allocator;

    public:

      typedef typename value_type_allocator::value_type value_type;

      typedef typename value_type_allocator::pointer pointer;

      typedef typename value_type_allocator::const_pointer const_pointer;

      typedef typename value_type_allocator::reference reference;

      typedef typename value_type_allocator::const_reference const_reference;

    };

    template<typename Key, typename Data, class Allocator>
    struct types_traits : public data_types_traits<
      Data,
      Allocator>,
public value_types_traits<
      Key,
      Data,
      Allocator>,
public exception_throw_types_traits<
      Key,
      Data>
    {

    public:

      typedef typename Allocator::template rebind<Key>::other key_allocator;

      typedef typename key_allocator::value_type key_type;

      typedef typename key_allocator::pointer key_pointer;

      typedef typename key_allocator::const_pointer const_key_pointer;

      typedef typename key_allocator::reference key_reference;

      typedef typename key_allocator::const_reference const_key_reference;

    };

  } // namespace detail

} // namespace pb_assoc

#endif // #ifndef TYPES_TRAITS_HPP
