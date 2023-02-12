// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <random>
#include <testsuite_common_types.h>

namespace __gnu_test
{
  struct constexpr_member_data
  {
    template<typename _Ttesttype>
      void
      operator()()
      {
	struct _Concept
	{
	  void __constraint()
	  {
	    constexpr auto v1 __attribute__((unused))
	      = _Ttesttype::word_size;
	    constexpr auto v2 __attribute__((unused))
	      = _Ttesttype::short_lag;
	    constexpr auto v3 __attribute__((unused))
	      = _Ttesttype::long_lag;
	    constexpr auto v4 __attribute__((unused))
	      = _Ttesttype::default_seed;
	  }
	};

	_Concept c;
	c.__constraint();
      }
  };
}

int main()
{
  __gnu_test::constexpr_member_data test;
  typedef std::ranlux24_base type;
  test.operator()<type>();
  return 0;
}
