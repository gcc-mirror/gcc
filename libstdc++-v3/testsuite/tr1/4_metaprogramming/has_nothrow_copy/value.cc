// 2004-12-30  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

// 4.5.3 Type properties

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::has_nothrow_copy;
  using namespace __gnu_test;

  VERIFY( (test_category<has_nothrow_copy, void>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int>(true)) );
  VERIFY( (test_category<has_nothrow_copy, float>(true)) );
  VERIFY( (test_category<has_nothrow_copy, EnumType>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int*>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int(*)(int)>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int (ClassType::*)>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int (ClassType::*) (int)>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int[2]>(true)) );
  VERIFY( (test_category<has_nothrow_copy, float[][3]>(true)) );
  VERIFY( (test_category<has_nothrow_copy, EnumType[2][3][4]>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int*[3]>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int(*[][2])(int)>(true)) );
  VERIFY( (test_category<has_nothrow_copy, int (ClassType::*[2][3])>(true)) );
  VERIFY( (test_category<has_nothrow_copy,
	   int (ClassType::*[][2][3]) (int)>(true)) );
}

int main()
{
  test01();
  return 0;
}
