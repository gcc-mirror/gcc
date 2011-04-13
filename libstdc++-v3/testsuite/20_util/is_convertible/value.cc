// { dg-options "-std=gnu++0x" }

// 2009-10-29  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2009, 2011 Free Software Foundation, Inc.
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
  using std::is_convertible;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_relationship<is_convertible, int, int>(true)) );
  VERIFY( (test_relationship<is_convertible, int, const int>(true)) );
  VERIFY( (test_relationship<is_convertible, volatile int, const int>(true)) );
  VERIFY( (test_relationship<is_convertible, int, float>(true)) );
  VERIFY( (test_relationship<is_convertible, double, float>(true)) );
  VERIFY( (test_relationship<is_convertible, float, int>(true)) );
  VERIFY( (test_relationship<is_convertible, int*, const int*>(true)) );
  VERIFY( (test_relationship<is_convertible, int*, void*>(true)) );
  VERIFY( (test_relationship<is_convertible, int[4], int*>(true)) );
  VERIFY( (test_relationship<is_convertible, float&, int>(true)) );
  VERIFY( (test_relationship<is_convertible, int, const int&>(true)) ); 
  VERIFY( (test_relationship<is_convertible, const int&, int>(true)) );
  VERIFY( (test_relationship<is_convertible, float, const int&>(true)) );
  VERIFY( (test_relationship<is_convertible, int(int), int(*)(int)>(true)) );
  VERIFY( (test_relationship<is_convertible, int(&)(int), int(*)(int)>(true)) );
  VERIFY( (test_relationship<is_convertible, EnumType, int>(true)) );
  VERIFY( (test_relationship<is_convertible, ClassType, ClassType>(true)) );
  VERIFY( (test_relationship<is_convertible, DerivedType, ClassType>(true)) );
  VERIFY( (test_relationship<is_convertible, DerivedType*, ClassType*>(true)) );
  VERIFY( (test_relationship<is_convertible, DerivedType&, ClassType&>(true)) );

  VERIFY( (test_relationship<is_convertible, const int, const int&>(true)) );

  VERIFY( (test_relationship<is_convertible, void, void>(true)) );
  VERIFY( (test_relationship<is_convertible, const void, void>(true)) );
  VERIFY( (test_relationship<is_convertible, void, volatile void>(true)) );
  VERIFY( (test_relationship<is_convertible, double&, ExplicitClass>(true)) );

  VERIFY( (test_relationship<is_convertible, int(int), int(&)(int)>(true)) );

  // Negative tests.
  VERIFY( (test_relationship<is_convertible, const int*, int*>(false)) );
  VERIFY( (test_relationship<is_convertible, int*, float*>(false)) );
  VERIFY( (test_relationship<is_convertible, const int[4], int*>(false)) );
  VERIFY( (test_relationship<is_convertible, int[4], int[4]>(false)) );
  VERIFY( (test_relationship<is_convertible, const int&, int&>(false)) );
  VERIFY( (test_relationship<is_convertible, float&, int&>(false)) );
  VERIFY( (test_relationship<is_convertible, float, volatile int&>(false)) );
  VERIFY( (test_relationship<is_convertible, int(int), int(int)>(false)) );
  VERIFY( (test_relationship<is_convertible, int(int), int(*)(void)>(false)) );
  VERIFY( (test_relationship<is_convertible, int(*)(int),
	                                     int(&)(int)>(false)) );
  VERIFY( (test_relationship<is_convertible, int, EnumType>(false)) );
  VERIFY( (test_relationship<is_convertible, int, ClassType>(false)) );
  VERIFY( (test_relationship<is_convertible, ClassType, DerivedType>(false)) );
  VERIFY( (test_relationship<is_convertible, ClassType*,
	                                     DerivedType*>(false)) );
  VERIFY( (test_relationship<is_convertible, ClassType&,
	                                     DerivedType&>(false)) );

  VERIFY( (test_relationship<is_convertible, void, int>(false)) );
  VERIFY( (test_relationship<is_convertible, void, float>(false)) );  
  VERIFY( (test_relationship<is_convertible, void, int(*)(int)>(false)) );

  // C++0x
  VERIFY( (test_relationship<is_convertible, int, void>(false)) );
  VERIFY( (test_relationship<is_convertible, int[4], void>(false)) );

  VERIFY( (test_relationship<is_convertible, int, int&>(false)) );
  VERIFY( (test_relationship<is_convertible, float,
	                                     volatile float&>(false)) );
  VERIFY( (test_relationship<is_convertible, const volatile int,
	                                     const volatile int&>(false)) );
  VERIFY( (test_relationship<is_convertible, volatile int,
	                                     volatile int&>(false)) );
  VERIFY( (test_relationship<is_convertible, int&, ExplicitClass>(false)) );
  VERIFY( (test_relationship<is_convertible, void*, ExplicitClass>(false)) );
}

int main()
{
  test01();
  return 0;
}
