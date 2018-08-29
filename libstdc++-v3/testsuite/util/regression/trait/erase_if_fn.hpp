// -*- C++ -*-

// Copyright (C) 2005-2018 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


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
 * @file erase_if_fn.hpp
 * Contains traits for a random regression test
 *    for a specific container type.
 */

#ifndef PB_DS_REGRESSION_TEST_ERASE_IF_FN_HPP
#define PB_DS_REGRESSION_TEST_ERASE_IF_FN_HPP

#include <functional>

namespace __gnu_pbds
{
  namespace test
  {
    namespace detail
    {
      template<typename T>
      struct regression_test_erase_if_fn
      : public std::unary_function<T, bool>
      {
      private:
	typedef const T&  const_reference;

      public:
	bool
        operator()(const_reference r_t) const
	{
	  return (r_t.length() == 0 ||
		  static_cast<size_t>(*((r_t).begin())) % 2 == 1);
	}
      };

      template<typename Hd, class Tl>
      struct regression_test_erase_if_fn<std::pair<Hd, Tl> >
      : public std::unary_function<std::pair<Hd, Tl>, bool>
      {
      private:
	typedef const std::pair<Hd, Tl>&  const_reference;
	typedef regression_test_erase_if_fn<Hd> hd_erase_if_fn;
	typedef regression_test_erase_if_fn<Tl> tl_erase_if_fn;

      public:
	bool
        operator()(const_reference r_t) const
	{
	  return (hd_erase_if_fn()(r_t.first) && tl_erase_if_fn()(r_t.second));
	}
      };
    } // namespace detail
  } // namespace test
} // namespace __gnu_pbds

#endif
