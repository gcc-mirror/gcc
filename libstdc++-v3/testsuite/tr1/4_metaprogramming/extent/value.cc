// 2004-12-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;
  using std::tr1::extent;
  using namespace __gnu_test;

  VERIFY( (test_property<extent, int, 0>(0)) );
  VERIFY( (test_property<extent, int[2], 0>(2)) );
  VERIFY( (test_property<extent, int[2][4], 0>(2)) );
  VERIFY( (test_property<extent, int[][4], 0>(0)) );
  VERIFY( (extent<int, 1>::value == 0) );
  VERIFY( (extent<int[2], 1>::value == 0) );
  VERIFY( (extent<int[2][4], 1>::value == 4) );
  VERIFY( (extent<int[][4], 1>::value == 4) );
  VERIFY( (extent<int[10][4][6][8][12][2], 4>::value == 12) );
  VERIFY( (test_property<extent, ClassType, 0>(0)) );
  VERIFY( (test_property<extent, ClassType[2], 0>(2)) );
  VERIFY( (test_property<extent, ClassType[2][4], 0>(2)) );
  VERIFY( (test_property<extent, ClassType[][4], 0>(0)) );
  VERIFY( (extent<ClassType, 1>::value == 0) );
  VERIFY( (extent<ClassType[2], 1>::value == 0) );
  VERIFY( (extent<ClassType[2][4], 1>::value == 4) );
  VERIFY( (extent<ClassType[][4], 1>::value == 4) );
  VERIFY( (extent<ClassType[10][4][6][8][12][2], 4>::value == 12) );
}

int main()
{
  test01();
  return 0;
}
