// 2004-12-30  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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
  using std::tr1::has_trivial_assign;
  using namespace __gnu_test;

  VERIFY( (test_category<has_trivial_assign, void>(true)) );
  VERIFY( (test_category<has_trivial_assign, int>(true)) );
  VERIFY( (test_category<has_trivial_assign, float>(true)) );
  VERIFY( (test_category<has_trivial_assign, EnumType>(true)) );
  VERIFY( (test_category<has_trivial_assign, int*>(true)) );
  VERIFY( (test_category<has_trivial_assign, int(*)(int)>(true)) );
  VERIFY( (test_category<has_trivial_assign, int (ClassType::*)>(true)) );
  VERIFY( (test_category<has_trivial_assign, int (ClassType::*) (int)>(true)) );
  VERIFY( (test_category<has_trivial_assign, int[2]>(true)) );
  VERIFY( (test_category<has_trivial_assign, float[][3]>(true)) );
  VERIFY( (test_category<has_trivial_assign, EnumType[2][3][4]>(true)) );
  VERIFY( (test_category<has_trivial_assign, int*[3]>(true)) );
  VERIFY( (test_category<has_trivial_assign, int(*[][2])(int)>(true)) );
  VERIFY( (test_category<has_trivial_assign, int (ClassType::*[2][3])>(true)) );
  VERIFY( (test_category<has_trivial_assign,
	   int (ClassType::*[][2][3]) (int)>(true)) );
}

int main()
{
  test01();
  return 0;
}
