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
#include <testsuite_hooks.h>

// Test std::to_chars error handling.

void
test01()
{
  char buf[9] = "********";
  std::to_chars_result r;

  r = std::to_chars(buf, buf, 1);
  VERIFY( r.ec == std::errc::value_too_large );
  VERIFY( r.ptr == buf );
  VERIFY( *r.ptr == '*' );

  r = std::to_chars(buf, buf + 3, 0b1000, 2);
  VERIFY( r.ec == std::errc::value_too_large );
  VERIFY( r.ptr == buf + 3 );
  VERIFY( *r.ptr == '*' );
  r = std::to_chars(buf, buf + 4, 0b1000, 2);
  VERIFY( r.ec == std::errc{} );
  VERIFY( r.ptr == buf + 4 );
  VERIFY( *r.ptr == '*' );

  r = std::to_chars(buf, buf + 4, 010000, 8);
  VERIFY( r.ec == std::errc::value_too_large );
  VERIFY( r.ptr == buf + 4 );
  VERIFY( *r.ptr == '*' );
  r = std::to_chars(buf, buf + 5, 010000, 8);
  VERIFY( r.ec == std::errc{} );
  VERIFY( r.ptr == buf + 5 );
  VERIFY( *r.ptr == '*' );

  r = std::to_chars(buf, buf + 5, 100000, 10);
  VERIFY( r.ec == std::errc::value_too_large );
  VERIFY( r.ptr == buf + 5 );
  VERIFY( *r.ptr == '*' );
  r = std::to_chars(buf, buf + 6, 100000, 10);
  VERIFY( r.ec == std::errc{} );
  VERIFY( r.ptr == buf + 6 );
  VERIFY( *r.ptr == '*' );

  r = std::to_chars(buf, buf + 6, 0x1000000, 16);
  VERIFY( r.ec == std::errc::value_too_large );
  VERIFY( r.ptr == buf + 6 );
  VERIFY( *r.ptr == '*' );
  r = std::to_chars(buf, buf + 7, 0x1000000, 16);
  VERIFY( r.ec == std::errc{} );
  VERIFY( r.ptr == buf + 7 );
  VERIFY( *r.ptr == '*' );
}

int
main()
{
  test01();
}
