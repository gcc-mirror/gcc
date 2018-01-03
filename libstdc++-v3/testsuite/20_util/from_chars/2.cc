// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// Test std::from_chars error handling.

void
test01()
{
  std::from_chars_result r;
  int i = 999;
  std::string_view s;

  s = "";
  r = std::from_chars(s.begin(), s.end(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  VERIFY( i == 999 );

  s = "*";
  r = std::from_chars(s.begin(), s.end(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  VERIFY( i == 999 );

  s = "-";
  r = std::from_chars(s.begin(), s.end(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  VERIFY( i == 999 );

  s = "-*";
  r = std::from_chars(s.begin(), s.end(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  VERIFY( i == 999 );

  unsigned u = 888;
  s = "-1";
  r = std::from_chars(s.begin(), s.end(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  s = "-a";
  r = std::from_chars(s.begin(), s.end(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  s = "-";
  r = std::from_chars(s.begin(), s.end(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.begin() );
  VERIFY( u == 888 );

  for (int base = 2; base <= 36; ++base)
  {
    const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz*";
    const char buf[2] = { '-', digits[base] };
    r = std::from_chars(buf, buf + 1, i, base);
    VERIFY( r.ec == std::errc::invalid_argument );
    VERIFY( r.ptr == buf );
    VERIFY( i == 999 );
    r = std::from_chars(buf + 1, buf + 2, i, base);
    VERIFY( r.ec == std::errc::invalid_argument );
    VERIFY( r.ptr == buf + 1 );
    VERIFY( i == 999 );
    r = std::from_chars(buf, buf + 2, i, base);
    VERIFY( r.ec == std::errc::invalid_argument );
    VERIFY( r.ptr == buf );
    VERIFY( i == 999 );
  }
}

void
test02()
{
  std::from_chars_result r;
  std::string_view s;

  signed char c = -5;
  s = "-10000001";
  r = std::from_chars(s.begin(), s.end(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "-10000001*";
  r = std::from_chars(s.begin(), s.end(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 9 );
  s = "-10000001000*";
  r = std::from_chars(s.begin(), s.end(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 12 );
  s = "-129";
  r = std::from_chars(s.begin(), s.end(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "-129*";
  r = std::from_chars(s.begin(), s.end(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 4 );
  s = "-100";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "-100*";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 4 );
  s = "-81";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "-81*";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 3 );
  s = "128";
  r = std::from_chars(s.begin(), s.end(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "128*";
  r = std::from_chars(s.begin(), s.end(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 3 );
  s = "80";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "80*";
  r = std::from_chars(s.begin(), s.end(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 2 );
  VERIFY( c == -5 );

  unsigned char uc = 9;
  s = "100000000";
  r = std::from_chars(s.begin(), s.end(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "100000000*";
  r = std::from_chars(s.begin(), s.end(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 9 );
  s = "100000000000*";
  r = std::from_chars(s.begin(), s.end(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 12 );
  s = "256";
  r = std::from_chars(s.begin(), s.end(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "256**";
  r = std::from_chars(s.begin(), s.end(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 3 );
  s = "256000**";
  r = std::from_chars(s.begin(), s.end(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 6 );
  s = "100";
  r = std::from_chars(s.begin(), s.end(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.end() );
  s = "100**";
  r = std::from_chars(s.begin(), s.end(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 3 );
  s = "100000**";
  r = std::from_chars(s.begin(), s.end(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 6 );
  VERIFY( uc == 9 );

  unsigned long long ull = 123;
  s = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz****";
  r = std::from_chars(s.begin(), s.end(), ull, 36);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.begin() + 42 );
  VERIFY( ull == 123 );
}

int
main()
{
  test01();
  test02();
}
