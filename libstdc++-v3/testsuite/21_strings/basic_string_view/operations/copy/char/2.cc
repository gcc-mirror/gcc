// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <string_view>
#include <testsuite_hooks.h>

using char_type = char;

// PR libstdc++/88084
// LWG 2777. basic_string_view::copy should use char_traits::copy

struct traits : std::char_traits<char_type>
{
  static char_type*
  copy(char_type* s, const char_type* p, std::size_t n)
  {
    while (n--)
      *s++ = 'X';
    return s;
  }
};

void
test01()
{
  std::basic_string_view<char_type, traits> s = "abc";
  char_type buf[3] = { '1', '2', '3' };
  auto len = s.copy(buf, 3, 1);
  VERIFY( len == 2 );
  VERIFY( buf[0] == 'X' );
  VERIFY( buf[1] == 'X' );
  VERIFY( buf[2] == '3' );
}

int
main()
{
  test01();
}
