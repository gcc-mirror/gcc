// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <regex>
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
	    constexpr auto v1(_Ttesttype::icase);
	    constexpr auto v2(_Ttesttype::nosubs);
	    constexpr auto v3(_Ttesttype::optimize);
	    constexpr auto v4(_Ttesttype::collate);
	    constexpr auto v5(_Ttesttype::ECMAScript);
	    constexpr auto v6(_Ttesttype::basic);
	    constexpr auto v7(_Ttesttype::extended);
	    constexpr auto v8(_Ttesttype::awk);
	    constexpr auto v9(_Ttesttype::grep);
	    constexpr auto v10(_Ttesttype::egrep);
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
  test.operator()<std::regex>();
  test.operator()<std::wregex>();
  return 0;
}
