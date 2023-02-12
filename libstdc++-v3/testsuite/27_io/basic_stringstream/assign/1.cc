// { dg-do run { target c++11 } }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// 27.8.5.2 Assign and swap [stringstream.assign]

#include <sstream>
#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringstream s1;
  s1 << "absence of a signal";
  std::string s;
  s1 >> s;

  std::stringstream s2;
  s2 = std::move(s1);
  s2 >> s;
  VERIFY(s == "of");

  std::stringstream s3;
  s3.swap(s2);
  s3 >> s;
  VERIFY(s == "a");

  swap(s1, s3);
  s1 >> s;
  VERIFY(s == "signal");

  s2 << "should never be used as a signal";
  s1 = std::move(s2);
  getline(s1, s);
  VERIFY(s == "should never be used as a signal");
  s3 = std::move(s1);
  VERIFY(s3.eof());
}

void test02()
{
  std::stringstream s0;
  s0 << " 1234.5 ";
  std::stringstream s;
  s = std::move(s0);
  char c{};
  s >> c;
  VERIFY( c == '1' );
  int i{};
  s >> i;
  VERIFY( i == 234 );
  double d{};
  s >> d;
  VERIFY( d == .5 );
}

void test03()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::wstringstream s0;
  s0 << L" 1234.5 ";
  std::wstringstream s;
  s = std::move(s0);
  s.swap(s0);
  swap(s, s0);
  wchar_t c{};
  s >> c;
  VERIFY( c == L'1' );
  int i{};
  s >> i;
  VERIFY( i == 234 );
  double d{};
  s >> d;
  VERIFY( d == .5 );
#endif
}

int
main()
{
  test01();
  test02();
  test03();
}
