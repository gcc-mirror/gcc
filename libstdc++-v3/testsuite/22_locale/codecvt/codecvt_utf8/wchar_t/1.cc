// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

#include <string>
#include <codecvt>
#include <testsuite_hooks.h>

void
test01()
{
  const auto out = (const char*)u8"\u00A33.50";
  wchar_t in[8] = {};
  std::codecvt_utf8<wchar_t> cvt;
  std::mbstate_t st;
  const char* no;
  wchar_t* ni;
  auto res = cvt.in(st, out, out+6, no, in, in+8, ni);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( in[1] == L'3' );
  VERIFY( in[2] == L'.' );
  VERIFY( in[3] == L'5' );
  VERIFY( in[4] == L'0' );

  char out2[8] = {};
  char* no2;
  const wchar_t* ni2;
  res = cvt.out(st, in, ni, ni2, out2, out2+8, no2);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( out2 == std::string(out) );
}

int
main()
{
  test01();
}
