// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// 2009-11-11  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2009-2023 Free Software Foundation, Inc.
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
  using namespace std;

  const string one(to_string(-2));
  VERIFY( one == "-2" );

  const string two(to_string(10u));
  VERIFY( two == "10" );

  const string three(to_string(2l));
  VERIFY( three == "2" );

  const string four(to_string(3000ul));
  VERIFY( four == "3000" );

  const string five(to_string(7ll));
  VERIFY( five == "7" );

  const string six(to_string(400ull));
  VERIFY( six == "400" );

  const string seven(to_string(-1.0F));
  VERIFY( seven == "-1.000000" );

  const string eight(to_string(2.0));
  VERIFY( eight == "2.000000" );

  const string nine(to_string(-4.0L));
  VERIFY( nine == "-4.000000" );
}

int main()
{
  test01();
  return 0;
}
