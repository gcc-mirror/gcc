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
 * @file native_type_trait.hpp
 * Containsert traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_NATIVE_TYPE_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_NATIVE_TYPE_TRAIT_HPP

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      template<typename Key, class Allocator>
      struct native_key_type;

      template<typename Allocator>
      struct native_key_type<
	basic_type,
	Allocator>
      {
	typedef std::string type;

	static type
        native_key(typename Allocator::template rebind<
		   basic_type>::other::const_reference r_key)
	{
	  return (std::string(r_key));
	}
      };

      template<typename Hd, class Tl, class Allocator>
      struct native_key_type<
	std::pair<
        Hd,
        Tl>,
	Allocator>
      {
	typedef typename native_key_type< Hd, Allocator>::type hd_type;

	typedef typename native_key_type< Tl, Allocator>::type tl_type;

	typedef std::pair< hd_type, tl_type> type;

	static type
        native_key(typename Allocator::template rebind<            std::pair<Hd, Tl> >::other::const_reference r_key)
	{
	  return (std::make_pair(
				 native_key_type<Hd, Allocator>::native_key(r_key.first),
				 native_key_type<Tl, Allocator>::native_key(r_key.second)));
	}
      };

      template<typename Native_Key_Type,
	       class Key_Type,
	       class Data_Type,
	       class Allocator>
      struct native_type_traits_base;

      template<typename Native_Key_Type, class Key_Type, class Allocator>
      struct native_type_traits_base<
	Native_Key_Type,
	Key_Type,
	basic_type,
	Allocator>
      {
      public:
	typedef std::map< Native_Key_Type, std::string> type;

      public:
	static const typename type::key_type& 
        extract_key(typename type::const_reference r_val)
	{
	  return (r_val.first);
	}

	static typename type::value_type
        native_value(typename Allocator::template rebind<            std::pair<Key_Type, basic_type> >::other::const_reference r_val)
	{
	  return (std::make_pair(
				 native_key_type<Key_Type, Allocator>::native_key(r_val.first),
				 std::string(r_val.second)));
	}
      };

      template<typename Native_Key_Type, class Key_Type, class Allocator>
      struct native_type_traits_base<
	Native_Key_Type,
	Key_Type,
	__gnu_pbds::null_mapped_type,
	Allocator>
      {
      public:
	typedef std::set< Native_Key_Type> type;

      public:
	static const typename type::key_type& 
        extract_key(typename type::const_reference r_val)
	{
	  return (r_val);
	}

	static typename type::value_type
        native_value(typename Allocator::template rebind<
		     Key_Type>::other::const_reference r_val)
	{
	  return (native_key_type<Key_Type, Allocator>::native_key(
								   r_val));
	}
      };

#define PB_DS_NATIVE_KEY_TYPE_C_DEC				\
      native_key_type<						\
						Key_Type,	\
						Allocator>

#define PB_DS_BASE_C_DEC						\
      native_type_traits_base<						\
									typename PB_DS_NATIVE_KEY_TYPE_C_DEC::type, \
									Key_Type, \
									Data_Type, \
									Allocator>

      template<typename Key_Type, class Data_Type, class Allocator>
      struct native_type_traits : public PB_DS_BASE_C_DEC
      {
	typedef typename PB_DS_BASE_C_DEC::type type;

	typedef typename type::key_type key_type;

	static typename PB_DS_NATIVE_KEY_TYPE_C_DEC::type
        native_key(typename Allocator::template rebind<
		   Key_Type>::other::const_reference r_key)
	{
	  return (PB_DS_NATIVE_KEY_TYPE_C_DEC::native_key(r_key));
	}
      };

#undef PB_DS_BASE_C_DEC

#undef PB_DS_NATIVE_KEY_TYPE_C_DEC

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_REGRESSION_TEST_NATIVE_TYPE_TRAIT_HPP
