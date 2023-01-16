// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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
  const char out[] = "abc";
  char16_t in[4];
  std::codecvt_utf8<char16_t> cvt;
  std::mbstate_t st;
  const char* no;
  char16_t* ni;
  auto res = cvt.in(st, out, out+3, no, in, in+3, ni);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( in[0] == u'a' );
  VERIFY( in[1] == u'b' );
  VERIFY( in[2] == u'c' );
}

void
test02()
{
  const char out[] = "abc";
  char16_t in[4];
  std::codecvt_utf8<char16_t, 0x10ffff, std::little_endian> cvt;
  std::mbstate_t st;
  const char* no;
  char16_t* ni;
  auto res = cvt.in(st, out, out+3, no, in, in+3, ni);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( in[0] == u'a' );
  VERIFY( in[1] == u'b' );
  VERIFY( in[2] == u'c' );
}

void
test03()
{
  const char out[] = "abc";
  char32_t in[4];
  std::codecvt_utf8<char32_t> cvt;
  std::mbstate_t st;
  const char* no;
  char32_t* ni;
  auto res = cvt.in(st, out, out+3, no, in, in+3, ni);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( in[0] == U'a' );
  VERIFY( in[1] == U'b' );
  VERIFY( in[2] == U'c' );
}

void
test04()
{
  const char out[] = "abc";
  char32_t in[4];
  std::codecvt_utf8<char32_t, 0x10ffff, std::little_endian> cvt;
  std::mbstate_t st;
  const char* no;
  char32_t* ni;
  auto res = cvt.in(st, out, out+3, no, in, in+3, ni);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( in[0] == U'a' );
  VERIFY( in[1] == U'b' );
  VERIFY( in[2] == U'c' );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
