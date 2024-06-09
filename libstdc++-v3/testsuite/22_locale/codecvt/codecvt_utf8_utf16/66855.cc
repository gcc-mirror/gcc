// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

void
test01()
{
  std::codecvt_utf8_utf16<char16_t> cvt;
  char16_t utf16[] = u"\ub098\ub294\ud0dc\uc624";
  const char16_t* nf16;
  char utf8[16];
  char* nt8;
  std::mbstate_t st{};
  auto res = cvt.out(st, utf16, utf16+4, nf16, utf8, utf8+16, nt8);
  VERIFY( res == std::codecvt_base::ok );

  st = {};
  char16_t buf[4] = {};
  const char* nf8 = nt8;
  char16_t* nt16;
  res = cvt.in(st, utf8, nf8, nf8, buf, buf+4, nt16);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( nt16 == buf+4 );
  VERIFY( buf[0] == utf16[0] );
  VERIFY( buf[1] == utf16[1] );
  VERIFY( buf[2] == utf16[2] );
  VERIFY( buf[3] == utf16[3] );
}

void
test02()
{
  // Endianness flag should make no difference.
  std::codecvt_utf8_utf16<char16_t, 0x10ffff, std::little_endian> cvt;
  char16_t utf16[] = u"\ub098\ub294\ud0dc\uc624";
  const char16_t* nf16;
  char utf8[16];
  char* nt8;
  std::mbstate_t st{};
  auto res = cvt.out(st, utf16, utf16+4, nf16, utf8, utf8+16, nt8);
  VERIFY( res == std::codecvt_base::ok );

  st = {};
  char16_t buf[4] = {};
  const char* nf8 = nt8;
  char16_t* nt16;
  res = cvt.in(st, utf8, nf8, nf8, buf, buf+4, nt16);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( nt16 == buf+4 );
  VERIFY( buf[0] == utf16[0] );
  VERIFY( buf[1] == utf16[1] );
  VERIFY( buf[2] == utf16[2] );
  VERIFY( buf[3] == utf16[3] );
}

int
main()
{
  test01();
  test02();
}
