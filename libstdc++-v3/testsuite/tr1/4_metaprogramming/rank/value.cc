// 2004-12-11  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
  using std::tr1::rank;
  using namespace __gnu_test;

  VERIFY( (test_property<rank, int>(0)) );
  VERIFY( (test_property<rank, int[2]>(1)) );
  VERIFY( (test_property<rank, int[][4]>(2)) );
  VERIFY( (test_property<rank, int[2][2][4][4][6][6]>(6)) );
  VERIFY( (test_property<rank, ClassType>(0)) );
  VERIFY( (test_property<rank, ClassType[2]>(1)) );
  VERIFY( (test_property<rank, ClassType[][4]>(2)) );
  VERIFY( (test_property<rank, ClassType[2][2][4][4][6][6]>(6)) );
}

int main()
{
  test01();
  return 0;
}
