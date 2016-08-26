// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// 2009-11-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

// DR 1261. Insufficient overloads for to_string / to_wstring
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  const wstring one(to_wstring(-2));
  VERIFY( one == L"-2" );

  const wstring two(to_wstring(10u));
  VERIFY( two == L"10" );

  const wstring three(to_wstring(2l));
  VERIFY( three == L"2" );

  const wstring four(to_wstring(3000ul));
  VERIFY( four == L"3000" );

  const wstring five(to_wstring(7ll));
  VERIFY( five == L"7" );

  const wstring six(to_wstring(400ull));
  VERIFY( six == L"400" );

  const wstring seven(to_wstring(-1.0F));
  VERIFY( seven == L"-1.000000" );

  const wstring eight(to_wstring(2.0));
  VERIFY( eight == L"2.000000" );

  const wstring nine(to_wstring(-4.0L));
  VERIFY( nine == L"-4.000000" );
}

int main()
{
  test01();
  return 0;
}
