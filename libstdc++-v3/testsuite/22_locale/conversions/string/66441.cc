// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// libstdc++/66441

#include <locale>
#include <codecvt>
#include <testsuite_hooks.h>

void
test01()
{
  // convert from UCS-4 to UTF16BE with BOM.
  using cvt = std::codecvt_utf16<char32_t, 0x10FFFF, std::generate_header>;
  std::wstring_convert<cvt, char32_t> conv;
  auto to = conv.to_bytes(U"ab\u00e7");

  VERIFY( to.length() == 8 );
  VERIFY( (unsigned char)to[0] == 0xfe );
  VERIFY( (unsigned char)to[1] == 0xff );
  VERIFY( (unsigned char)to[2] == 0x00 );
  VERIFY( (unsigned char)to[3] == 0x61 );
  VERIFY( (unsigned char)to[4] == 0x00 );
  VERIFY( (unsigned char)to[5] == 0x62 );
  VERIFY( (unsigned char)to[6] == 0x00 );
  VERIFY( (unsigned char)to[7] == 0xe7 );
}

int
main()
{
  test01();
}
