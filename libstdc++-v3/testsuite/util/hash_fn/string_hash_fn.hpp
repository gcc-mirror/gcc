// -*- C++ -*-

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
 * @file string_hash_fn.hpp
 * Contains a string hash function.
 */

#ifndef PB_DS_STRING_HASH_FN_HPP
#define PB_DS_STRING_HASH_FN_HPP

#include <string>

namespace __gnu_pbds
{
  namespace test
  {
    struct string_hash_fn
    {
      template<typename String_T>
      inline size_t
      operator()(const String_T& r_str) const
      {
	typedef typename String_T::const_iterator const_iterator;
	const_iterator b = r_str.begin();
	const_iterator e = r_str.end();

	size_t ret = 0;
	while (b != e)
	  {
	    ret *= 5;
	    ret += static_cast<size_t>(*(b++));
	  }
	return ret;
      }
    };
  } // namespace test
} // namespace __gnu_pbds

#endif
