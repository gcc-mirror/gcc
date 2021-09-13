// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <charconv>
#include <string>
#include <cmath>
#include <testsuite_hooks.h>

// Test std::from_chars error handling.

#if __cpp_lib_to_chars >= 201611L
void
test01()
{
  std::from_chars_result r;
  double d = 3.2;
  std::string s;

  for (auto p : { "", "*", ".", "-", "-*", "-.", "+", "+.", "+-", "-+", "+1",
		  ".p1", "-.p1",
		  "in", "inch", "+inf", "na", "nam", "+nan" })
  {
    s = p;
    for (auto fmt : { std::chars_format::fixed, std::chars_format::scientific,
		      std::chars_format::general, std::chars_format::hex })
    {
      r = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
      VERIFY( r.ec == std::errc::invalid_argument );
      VERIFY( r.ptr == s.data() );
      VERIFY( d == 3.2 );
    }
  }

  for (auto p : { ".e1", "-.e1" }) // These are valid patterns for hex format
  {
    s = p;
    for (auto fmt : { std::chars_format::fixed, std::chars_format::scientific,
		      std::chars_format::general })
    {
      r = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
      VERIFY( r.ec == std::errc::invalid_argument );
      VERIFY( r.ptr == s.data() );
      VERIFY( d == 3.2 );
    }
  }

  // scientific format requires an exponent
  for (auto p : { "1.2", "-1.2", "1.2e", "-1.2e", "1.2e-", "-1.2e+" })
  {
    s = p;
    r = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::scientific);
    VERIFY( r.ec == std::errc::invalid_argument );
    VERIFY( r.ptr == s.data() );
    VERIFY( d == 3.2 );
  }

  // patterns that are invalid without the final character
  for (auto p : { "1", ".1", "-1", "-.1",
		  "inf", "-inf", "nan", "-nan" })
  {
    s = p;
    for (auto fmt : { std::chars_format::fixed, std::chars_format::scientific,
		      std::chars_format::general, std::chars_format::hex })
    {
      r = std::from_chars(s.data(), s.data() + s.length() - 1, d, fmt);
      VERIFY( r.ec == std::errc::invalid_argument );
      VERIFY( r.ptr == s.data() );
      VERIFY( d == 3.2 );
    }
  }
}

void
test02()
{
  std::from_chars_result r;
  std::string s;

  float f = 0.5;
  // Overflow
  s = "99999999999999999e999999999999999999";
  r = std::from_chars(s.data(), s.data() + s.length(), f);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  VERIFY( f == 0.5 );

  s += '*';
  r = std::from_chars(s.data(), s.data() + s.length(), f);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() - 1 );
  VERIFY( f == 0.5 );

  s.insert(s.begin(), '-');
  r = std::from_chars(s.data(), s.data() + s.length(), f);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() - 1 );
  VERIFY( f == 0.5 );
}

void
test03()
{
  double d = 0.5;
  // Underflow
  std::string s("-1.2345e-9999zzz");
  std::from_chars_result res;
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( res.ptr == s.data() + s.length() - 3 );
  VERIFY( res.ec == std::errc::result_out_of_range );
  VERIFY( d == 0.5 );

  res = std::from_chars(s.data() + 1, s.data() + s.length(), d);
  VERIFY( res.ptr == s.data() + s.length() - 3 );
  VERIFY( res.ec == std::errc::result_out_of_range );
  VERIFY( d == 0.5 );
}

void
test04()
{
  std::from_chars_result res;
  std::string z(2000, '0');
  // Invalid inputs for scientific format
  for (const char* s : { "", "1", ".", ".0", ".5", "1e+", "1e+-1" })
  {
    for (auto len : { 0, 10, 100, 1000, 2000 })
    {
      auto str = z.substr(len) + s;
      double d = 99.0;
      res = std::from_chars(str.data(), str.data() + str.length(), d,
			    std::chars_format::scientific);
      VERIFY( res.ec == std::errc::invalid_argument );
      VERIFY( res.ptr == str.data() );
      VERIFY( d == 99.0 );
    }
  }
}
#endif

int
main()
{
#if __cpp_lib_to_chars >= 201611L
  test01();
  test02();
  test03();
  test04();
#endif
}
