// -*- C++ -*-

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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
 * @file to_string.hpp
 * Contains classes for transforming stuff to strings.
 */

#ifndef PB_DS_REGRESSION_TEST_TO_STRING_HPP
#define PB_DS_REGRESSION_TEST_TO_STRING_HPP

#include <regression/basic_type.hpp>
#include <string>

namespace __gnu_pbds
{

  namespace test
  {

    namespace detail
    {

      std::string
      to_string(const basic_type& r_t)
      {
	return (r_t);
      }

      std::string
      to_string(const std::string& r_t)
      {
	return (r_t);
      }

      template<typename Hd, class Tl>
      std::string
      to_string(const std::pair<Hd, Tl>&r_t)
      {
	std::ostringstream ret;

	ret << to_string(r_t.first) << " " << to_string(r_t.second);

	return (ret.str());
      }

    } // namespace detail

  } // namespace test

} // namespace __gnu_pbds

#endif // #ifndef PB_DS_REGRESSION_TEST_TO_STRING_HPP
