// 2010-12-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//
// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  using std::wstring;
  using std::move;

  wstring s01(L"abc");
  s01.reserve(30);
  wstring s02(L"def");
  s02.reserve(30); 
  VERIFY( move(s01) + move(s02) == wstring(L"abcdef") );

  wstring s03(L"abcdefghijklmnopqrstuvw");
  wstring s04(L"xyz");
  s04.reserve(30); 
  VERIFY( move(s03) + move(s04) == wstring(L"abcdefghijklmnopqrstuvwxyz") );

  wstring s05(L"abc");
  s05.reserve(30);
  wstring s06(L"defghijklmnopqrstuvwxyz");
  VERIFY( move(s05) + move(s06) == wstring(L"abcdefghijklmnopqrstuvwxyz") );

  const wstring sc1(L"abc");
  wstring s07(L"def");
  s07.reserve(30);
  VERIFY( sc1 + move(s07) == wstring(L"abcdef") );

  const wstring sc2(L"def");
  wstring s08(L"abc");
  s08.reserve(30);
  VERIFY( move(s08) + sc2 == wstring(L"abcdef") );
  
  wstring s09(L"abc");
  s09.reserve(30);
  VERIFY( move(s09) + L'd' == wstring(L"abcd") );

  wstring s10(L"abc");
  s10.reserve(30);
  VERIFY( move(s10) + L"def" == wstring(L"abcdef") );

  wstring s11(L"bcd");
  s11.reserve(30);
  VERIFY( L'a' + move(s11) == wstring(L"abcd") );

  wstring s12(L"def");
  s12.reserve(30);
  VERIFY( L"abc" + move(s12) == wstring(L"abcdef") );
}

int main()
{
  test01();
  return 0;
}
