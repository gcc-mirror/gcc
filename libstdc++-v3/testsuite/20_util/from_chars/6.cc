// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// <charconv> is supported in C++14 as a GNU extension
// { dg-do run { target c++14 } }
// { dg-add-options ieee }

#include <charconv>
#include <string>
#include <cfenv>
#include <testsuite_hooks.h>

void
test01()
{
#if __cpp_lib_to_chars >= 201611L
#if _GLIBCXX_USE_C99_FENV_TR1
  double d;
#ifdef FE_DOWNWARD
  std::fesetround(FE_DOWNWARD);
#endif
  const std::string s = "0.099999999999999999999999999";
  auto res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( res.ec == std::errc{} );
  VERIFY( res.ptr == s.data() + s.length() );
  // std::from_chars should ignore the current rounding mode
  // and always round to nearest.
  VERIFY( d == (double) 0.1 );
#endif
#endif
}

int
main()
{
  test01();
}
