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
 * @file type_trait.hpp
 * Containsert traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_TYPE_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_TYPE_TRAIT_HPP

#include <regression/basic_type.hpp>

namespace pb_ds
{

  namespace test
  {

    namespace detail
    {

      template<typename Cntnr>
      struct regression_test_type_traits
      {

      public:

	typedef Cntnr cntnr;

	typedef typename cntnr::key_type key_type;

	typedef typename cntnr::const_key_reference const_key_reference;

	typedef typename cntnr::value_type value_type;

	typedef typename cntnr::const_reference const_reference;

	typedef typename cntnr::mapped_type mapped_type;

	typedef typename cntnr::const_mapped_reference const_mapped_reference;

      public:

	template<typename Gen>
	static key_type
        generate_key(Gen& r_gen, size_t max)
	{
	  return (basic_type(r_gen, max));
	}

	template<typename Gen>
	static value_type
        generate_value(Gen& r_gen, size_t max)
	{
	  return (generate_value(r_gen, max,
				 pb_ds::detail::type_to_type<value_type>()));
	}

	static const_key_reference
        extract_key(const_reference r_val)
	{
	  return extract_key_imp(r_val);
	}

      private:
	typedef
        typename Cntnr::allocator::template rebind<
	basic_type>::other::const_reference
        basic_type_const_reference;

	typedef
        typename Cntnr::allocator::template rebind<
	  std::pair<
	  basic_type,
	  basic_type> >::other::const_reference
        basic_type_pair_const_reference;

      private:
	template<typename Gen>
	static value_type
        generate_value(Gen& r_gen, size_t max,  pb_ds::detail::type_to_type<pb_ds::null_mapped_type>)
	{
	  return (basic_type(r_gen, max));
	}

	template<typename Gen>
	static value_type
        generate_value(Gen& r_gen, size_t max, pb_ds::detail::type_to_type<basic_type>)
	{
	  return (basic_type(r_gen, max));
	}

	template<typename Gen>
	static value_type
        generate_value(Gen& r_gen, size_t max, pb_ds::detail::type_to_type<std::pair<const basic_type, basic_type> >)
	{
	  return (std::make_pair(basic_type(r_gen, max), basic_type(r_gen, max)));
	}

	static const_key_reference
        extract_key_imp(basic_type_const_reference r_val)
	{
	  return r_val;
	}

	static const_key_reference
        extract_key_imp(basic_type_pair_const_reference r_val)
	{
	  return r_val.first;
	}
      };

#undef PB_DS_BASE_C_DEC

    } // namespace detail

  } // namespace test

} // namespace pb_ds

#endif // #ifndef PB_DS_REGRESSION_TEST_TYPE_TRAIT_HPP
