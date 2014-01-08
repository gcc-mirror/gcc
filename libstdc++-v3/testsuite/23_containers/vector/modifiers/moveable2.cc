// { dg-options "-std=gnu++0x" }

// 2011-06-07  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace __gnu_test;

  std::vector<throwing_move_constructor> v1;

  throwing_move_constructor tmc;

  v1.push_back(tmc);
  VERIFY( v1.size() == 1 );

  v1.push_back(tmc);
  VERIFY( v1.size() == 2 );

  v1.insert(v1.end(), tmc);
  VERIFY( v1.size() == 3 );

  v1.insert(v1.end(), 100, tmc);
  VERIFY( v1.size() == 103 );

  v1.insert(v1.end(), 10, tmc);
  VERIFY( v1.size() == 113 );

  v1.insert(v1.end(), 1, tmc);
  VERIFY( v1.size() == 114 );

  std::vector<throwing_move_constructor> v2;

  throwing_move_constructor tmca[]
    = { throwing_move_constructor(), throwing_move_constructor(),
	throwing_move_constructor(), throwing_move_constructor() };

  v2.insert(v2.end(), tmca, tmca + 1);
  VERIFY( v2.size() == 1 );

  v2.insert(v2.end(), tmca, tmca + 4);
  VERIFY( v2.size() == 5 );

  v2.insert(v2.end(), tmca, tmca + 2);
  VERIFY( v2.size() == 7 );

  v2.insert(v2.end(), tmca, tmca + 1);
  VERIFY( v2.size() == 8 );
}

int main()
{
  test01();
  return 0;
}
