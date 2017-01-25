// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <codecvt>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  constexpr auto mode = generate_header;
  codecvt_utf16<char32_t, 0x10ffff, mode> cvt;
  mbstate_t state{};
  const char32_t* from = U"ABC";
  const char32_t* from_next;
  char to[100];
  char* to_next;

  cvt.out(state, from, from + 3, from_next, to, to + 100, to_next);

  VERIFY((unsigned char)to[0] == 0xfe);
  VERIFY((unsigned char)to[1] == 0xff);
  VERIFY(to[2] == 0x00);
  VERIFY(to[3] == 0x41);
  VERIFY(to[4] == 0x00);
  VERIFY(to[5] == 0x42);
  VERIFY(to[6] == 0x00);
  VERIFY(to[7] == 0x43);
}

void
test02()
{
  constexpr auto mode = codecvt_mode(generate_header|little_endian);
  codecvt_utf16<char32_t, 0x10ffff, mode> cvt;
  mbstate_t state{};
  const char32_t* from = U"ABC";
  const char32_t* from_next;
  char to[100];
  char* to_next;

  cvt.out(state, from, from + 3, from_next, to, to + 100, to_next);

  VERIFY((unsigned char)to[0] == 0xff);
  VERIFY((unsigned char)to[1] == 0xfe);
  VERIFY(to[2] == 0x41);
  VERIFY(to[3] == 0x00);
  VERIFY(to[4] == 0x42);
  VERIFY(to[5] == 0x00);
  VERIFY(to[6] == 0x43);
  VERIFY(to[7] == 0x00);
}

int
main()
{
  test01();
  test02();
}
