// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// 2009-06-05  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

// C++0X [28.10.1] class template match_results constructor

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

// Tests default constructor of the match_result class.
void test01()
{
  std::wcmatch cm;
  VERIFY( cm.size() == 0 );
  VERIFY( !cm.ready() );
  VERIFY( cm.empty() );
  VERIFY( cm.begin() == cm.end() ); // PR libstdc++/83600
}

void test02()
{
  std::wsmatch sm;
  VERIFY( sm.size() == 0 );
  VERIFY( !sm.ready() );
  VERIFY( sm.empty() );
  VERIFY( sm.begin() == sm.end() ); // PR libstdc++/83600
}

void test03()
{
  // P0935R0
  __gnu_test::implicitly_default_constructible test;
  test.operator()<std::wcmatch>();
  test.operator()<std::wsmatch>();
}

int
main()
{
  test01();
  test02();
  test03();
}
