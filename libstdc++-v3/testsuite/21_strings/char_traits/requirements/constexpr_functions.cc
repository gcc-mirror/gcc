// { dg-do compile { target c++11 } }

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

#include <string>
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
	    typedef typename _Ttesttype::char_type char_type;
	    typedef typename _Ttesttype::int_type int_type;
	    const char_type c1(0);
	    const char_type c2 = c1;
	    const int_type i(0);
	    constexpr auto v1 __attribute__((unused))
	      = _Ttesttype::eq(c1, c2);
	    constexpr auto v2 __attribute__((unused))
	      = _Ttesttype::lt(c1, c2);
	    constexpr auto v3 __attribute__((unused))
	      = _Ttesttype::to_char_type(i);
	    constexpr auto v4 __attribute__((unused))
	      = _Ttesttype::to_int_type(c1);
	    constexpr auto v5 __attribute__((unused))
	      = _Ttesttype::eq_int_type(i, i);
	    constexpr auto v6 __attribute__((unused))
	      = _Ttesttype::eof();
	    constexpr auto v7 __attribute__((unused))
	      = _Ttesttype::not_eof(i);
	  }
	};

	_Concept c;
	c.__constraint();
      }
  };
}

int main()
{
  __gnu_test::constexpr_member_functions test;
  test.operator()<std::char_traits<char>>();
  test.operator()<std::char_traits<wchar_t>>();
#ifdef _GLIBCXX_USE_CHAR8_T
  test.operator()<std::char_traits<char8_t>>();
#endif
  test.operator()<std::char_traits<char16_t>>();
  test.operator()<std::char_traits<char32_t>>();
  return 0;
}
