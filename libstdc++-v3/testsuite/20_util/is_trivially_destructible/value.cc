// { dg-options "-std=gnu++11" }
//
// 2012-04-15  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2012 Free Software Foundation, Inc.
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

#include <type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::is_trivially_destructible;
  using namespace __gnu_test;

  VERIFY( (test_category<is_trivially_destructible, int>(true)) );
  VERIFY( (test_category<is_trivially_destructible, TType>(true)) );
  VERIFY( (test_category<is_trivially_destructible, PODType>(true)) );

  VERIFY( (test_category<is_trivially_destructible, NType>(false)) );
  VERIFY( (test_category<is_trivially_destructible, SLType>(false)) );
}

int main()
{
  test01();
  return 0;
}
