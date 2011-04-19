// { dg-options "-std=gnu++0x" }

// 2010-06-09  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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
  using std::is_nothrow_constructible;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass,
	   double&>(true)) );
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass,
	   int&>(true)) );
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass,
	   double&, int&, double&>(true)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass,
	   double&>(true)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass,
	   int&>(true)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass,
	   double&, int&, double&>(true)) );

  VERIFY( (test_property<is_nothrow_constructible, int[1]>(true)) );

  // Negative tests.
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass,
	   void*>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass>
	   (false)) );
  VERIFY( (test_property<is_nothrow_constructible, NoexceptExplicitClass,
	   int, double>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass,
	   void*>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass>
	   (false)) );
  VERIFY( (test_property<is_nothrow_constructible, NothrowExplicitClass,
	   int, double>(false)) );

  VERIFY( (test_property<is_nothrow_constructible, ExceptExplicitClass,
	   double&>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, ExceptExplicitClass,
	   int&>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, ExceptExplicitClass,
	   double&, int&, double&>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, ThrowExplicitClass,
	   double&>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, ThrowExplicitClass,
	   int&>(false)) );
  VERIFY( (test_property<is_nothrow_constructible, ThrowExplicitClass,
	   double&, int&, double&>(false)) );

  VERIFY( (test_property<is_nothrow_constructible, int[]>(false)) );
}

int main()
{
  test01();
  return 0;
}
