// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

#include <ratio>
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
	    constexpr intmax_t v1 __attribute__((unused))
	      = _Ttesttype::num;
	    constexpr intmax_t v2 __attribute__((unused))
	      = _Ttesttype::den;
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
  typedef std::ratio<600, 900> R1;
  typedef std::ratio<700, 200> R2;
  test.operator()<R1>();
  test.operator()<std::ratio_add<R1, R2>>();
  test.operator()<std::ratio_subtract<R1, R2>>();
  test.operator()<std::ratio_multiply<R1, R2>>();
  test.operator()<std::ratio_divide<R1, R2>>();
  return 0;
}
