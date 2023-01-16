// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

// 21.4 Numeric Conversions [string.conversions]

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
#if _GLIBCXX_USE_C99_WCHAR

  using namespace std;

  long long ll1 = -2;
  wstring one(to_wstring(ll1));
  VERIFY( one == L"-2" );

  long long ll2 = 10;
  wstring two(to_wstring(ll2));
  VERIFY( two == L"10" );

  unsigned long long ull1 = 2;
  wstring three(to_wstring(ull1));
  VERIFY( three == L"2" );

  unsigned long long ull2 = 3000;
  wstring four(to_wstring(ull2));
  VERIFY( four == L"3000" );

  long double ld1 = 2.0L;
  wstring five(to_wstring(ld1));
  VERIFY( five == L"2.000000" );

  long double ld2 = -4.0L;
  wstring six(to_wstring(ld2));
  VERIFY( six == L"-4.000000" );

#endif
}

int main()
{
  test01();
  return 0;
}
