// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// Test std::from_chars error handling.

void
test01()
{
  std::from_chars_result r;
  int i = 999;
  std::string s;

  s = "";
  r = std::from_chars(s.data(), s.data() + s.length(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  VERIFY( i == 999 );

  s = "*";
  r = std::from_chars(s.data(), s.data() + s.length(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  VERIFY( i == 999 );

  s = "-";
  r = std::from_chars(s.data(), s.data() + s.length(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  VERIFY( i == 999 );

  s = "-*";
  r = std::from_chars(s.data(), s.data() + s.length(), i);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  VERIFY( i == 999 );

  unsigned u = 888;
  s = "-1";
  r = std::from_chars(s.data(), s.data() + s.length(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  s = "-a";
  r = std::from_chars(s.data(), s.data() + s.length(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
  s = "-";
  r = std::from_chars(s.data(), s.data() + s.length(), u);
  VERIFY( r.ec == std::errc::invalid_argument );
  VERIFY( r.ptr == s.data() );
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
  std::string s;

  signed char c = -5;
  s = "-10000001";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "-10000001*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 9 );
  s = "-10000001000*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 12 );
  s = "-129";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "-129*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 4 );
  s = "-100";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "-100*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 4 );
  s = "-81";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "-81*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 3 );
  s = "128";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "128*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 3 );
  s = "80";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "80*";
  r = std::from_chars(s.data(), s.data() + s.length(), c, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 2 );
  VERIFY( c == -5 );

  unsigned char uc = 9;
  s = "100000000";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "100000000*";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 9 );
  s = "100000000000*";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 2);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 12 );
  s = "256";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "256**";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 3 );
  s = "256000**";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 10);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 6 );
  s = "100";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + s.length() );
  s = "100**";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 3 );
  s = "100000**";
  r = std::from_chars(s.data(), s.data() + s.length(), uc, 16);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 6 );
  VERIFY( uc == 9 );

  unsigned long long ull = 123;
  s = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz****";
  r = std::from_chars(s.data(), s.data() + s.length(), ull, 36);
  VERIFY( r.ec == std::errc::result_out_of_range );
  VERIFY( r.ptr == s.data() + 42 );
  VERIFY( ull == 123 );
}

int
main()
{
  test01();
  test02();
}
