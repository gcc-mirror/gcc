// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <chrono>
#include <testsuite_common_types.h>

namespace __gnu_test
{
  struct constexpr_member_functions
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
	      = _Ttesttype::min();
	    constexpr auto v2 __attribute__((unused))
	      = _Ttesttype::max();

	    constexpr _Ttesttype obj;
	    constexpr auto v3 __attribute__((unused))
	      = obj.time_since_epoch();
	  }
	};

	_Concept c;
	c.__constraint();
      }
  };
}

int main()
{
  using namespace std::chrono;
  __gnu_test::constexpr_member_functions test;
  test.operator()<time_point<system_clock>>();
  return 0;
}
