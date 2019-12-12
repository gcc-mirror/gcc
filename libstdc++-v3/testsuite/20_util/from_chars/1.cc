// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <charconv>
#include <string_view>

template<typename I>
bool
check_from_chars(I expected, std::string_view s, int base = 0, char term = '\0')
{
  I val;
  std::from_chars_result r = base == 0
    ? std::from_chars(s.begin(), s.end(), val)
    : std::from_chars(s.begin(), s.end(), val, base);
  return r.ec == std::errc{} && (r.ptr == s.end() || *r.ptr == term) && val == expected;
}

#include <climits>
#include <testsuite_hooks.h>

void
test01()
{
  // Using base 10
  VERIFY( check_from_chars(123, "123") );
  VERIFY( check_from_chars(-123, "-123") );
  VERIFY( check_from_chars(123, "123a", 10, 'a') );
  VERIFY( check_from_chars(123, "0000000000000000000000000000123") );
  VERIFY( check_from_chars(123, "0000000000000000000000000000123a", 10, 'a') );
}

void
test02()
{
  // "0x" parsed as "0" not as hex prefix:
  VERIFY( check_from_chars(0, "0x1", 10, 'x') );
  VERIFY( check_from_chars(0, "0X1", 10, 'X') );
  VERIFY( check_from_chars(0, "0x1", 16, 'x') );
  VERIFY( check_from_chars(0, "0X1", 16, 'X') );

  VERIFY( check_from_chars(1155, "xx", 34) );
  VERIFY( check_from_chars(1155, "XX", 34) );
  VERIFY( check_from_chars(1155, "Xx", 34) );
  VERIFY( check_from_chars(1224, "yy", 35) );
  VERIFY( check_from_chars(1224, "YY", 35) );
  VERIFY( check_from_chars(1224, "yY", 35) );
  VERIFY( check_from_chars(1295, "zz", 36) );
  VERIFY( check_from_chars(1295, "ZZ", 36) );
  VERIFY( check_from_chars(1295, "Zz", 36) );

  // Parsing stops at first invalid digit for the given base:
  VERIFY( check_from_chars(1, "01234", 2, '2') );
  VERIFY( check_from_chars(27, "1234", 4, '4') );
  VERIFY( check_from_chars(1155, "xxy", 34, 'y') );
  VERIFY( check_from_chars(1224, "yyz", 35, 'z') );
}

int
main()
{
  test01();
  test02();
}
