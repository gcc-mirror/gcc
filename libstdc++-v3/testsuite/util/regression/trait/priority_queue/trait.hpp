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
 * @file trait.hpp
 * Containsert traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_TRAIT_HPP
#define PB_DS_REGRESSION_TEST_TRAIT_HPP

#include <regression/trait/erase_if_fn.hpp>

namespace pb_ds
{
  namespace test
  {
    namespace detail
    {
      template<typename Cntnr>
      struct regression_test_traits
      {
	typedef typename Cntnr::value_type value_type;
	typedef typename Cntnr::const_reference const_reference;
	typedef pb_ds::test::native_priority_queue<std::string, true> native_type;
	typedef typename native_type::value_type native_value_type;

	template<typename T>
	struct erase_if_fn : public regression_test_erase_if_fn<T>
	{ };

	template<typename Gen>
	static value_type
        generate_value(Gen& r_gen, size_t max)
	{ return basic_type(r_gen, max); }

	static native_value_type
        native_value(const_reference r_val)
	{ return native_value_type(r_val); }

	static bool
        cmp(const_reference r_val, const native_value_type& r_native_val)
	{ return val_to_string(r_val) == r_native_val; }

	static std::string
        val_to_string(const_reference r_val)
	{ return std::string(r_val); }
      };
    } // namespace detail
  } // namespace test
} // namespace pb_ds

#endif 
