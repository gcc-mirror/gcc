// { dg-options "-std=gnu++11" }
// 2013-05-02  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2013 Free Software Foundation, Inc.
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
  using std::is_null_pointer;
  using namespace __gnu_test;

  VERIFY( (test_category<is_null_pointer, std::nullptr_t>(true)) );

  VERIFY( (test_category<is_null_pointer, int>(false)) );
  VERIFY( (test_category<is_null_pointer, float>(false)) );
  VERIFY( (test_category<is_null_pointer, EnumType>(false)) );
  VERIFY( (test_category<is_null_pointer, int*>(false)) );
  VERIFY( (test_category<is_null_pointer, int(*)(int)>(false)) );
  VERIFY( (test_category<is_null_pointer, int (ClassType::*)>(false)) );
  VERIFY( (test_category<is_null_pointer, int (ClassType::*) (int)>(false)) );
  VERIFY( (test_category<is_null_pointer, int[2]>(false)) );
  VERIFY( (test_category<is_null_pointer, float[][3]>(false)) );
  VERIFY( (test_category<is_null_pointer, EnumType[2][3][4]>(false)) );
  VERIFY( (test_category<is_null_pointer, int*[3]>(false)) );
  VERIFY( (test_category<is_null_pointer, int(*[][2])(int)>(false)) );
  VERIFY( (test_category<is_null_pointer, int (ClassType::*[2][3])>(false)) );
  VERIFY( (test_category<is_null_pointer,
	   int (ClassType::*[][2][3]) (int)>(false)) );
  VERIFY( (test_category<is_null_pointer, ClassType>(false)) );
  VERIFY( (test_category<is_null_pointer, PODType>(false)) );
  VERIFY( (test_category<is_null_pointer, void>(false)) );
  VERIFY( (test_category<is_null_pointer, NType>(false)) );
  VERIFY( (test_category<is_null_pointer, TType>(false)) );
  VERIFY( (test_category<is_null_pointer, SLType>(false)) );
}

int main()
{
  test01();
  return 0;
}
