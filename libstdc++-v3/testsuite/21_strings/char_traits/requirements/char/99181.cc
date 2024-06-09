// { dg-do run { target c++17 } }

// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void test01()
{
  const char *a = "\x7f";
  const char *b = "\x80";
  int c = std::char_traits<char>::compare(a, b, 2);
  constexpr int d = std::char_traits<char>::compare("\x7f", "\x80", 2);

  VERIFY( c && (c < 0) == (static_cast<unsigned char>(a[0])
			   < static_cast<unsigned char>(b[0])) );
  VERIFY( d && (c < 0) == (d < 0) );
}

int main()
{
  test01();
  return 0;
}
