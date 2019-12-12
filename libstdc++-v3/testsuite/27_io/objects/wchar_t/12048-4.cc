// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

#include <iostream>
#include <cstdio>
#include <cwchar>
#include <testsuite_hooks.h>

void
test01()
{
  VERIFY( std::freopen("cin_unget-1.txt", "r", stdin) );

  wchar_t buf[2];
  VERIFY( std::wcin.rdbuf()->sgetn(buf, 2) == 2 );
  wint_t c1 = std::wcin.rdbuf()->sungetc();
  wint_t c2 = std::fgetwc(stdin);
  VERIFY( c1 == std::char_traits<wchar_t>::to_int_type(buf[1]) );
  VERIFY( c2 == c1 );
}

int main(void)
{
  test01();
  return 0;
}
