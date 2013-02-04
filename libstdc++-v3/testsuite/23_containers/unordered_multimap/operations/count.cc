// { dg-options "-std=gnu++0x" }

// 2011-10-28  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
//

#include <unordered_map>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  typedef unordered_multimap<int, int>::value_type value_type;

  unordered_multimap<int, int> umm0;
  VERIFY( umm0.count(0) == 0 );
  VERIFY( umm0.count(1) == 0 );

  umm0.insert(value_type(1, 1));
  VERIFY( umm0.count(0) == 0 );
  VERIFY( umm0.count(1) == 1 );

  umm0.insert(value_type(1, 2));
  VERIFY( umm0.count(0) == 0 );
  VERIFY( umm0.count(1) == 2 );

  umm0.insert(value_type(2, 1));
  VERIFY( umm0.count(2) == 1 );

  umm0.insert(value_type(3, 1));
  umm0.insert(value_type(3, 2));
  umm0.insert(value_type(3, 3));
  VERIFY( umm0.count(3) == 3 );

  umm0.erase(2);
  VERIFY( umm0.count(2) == 0 );

  umm0.erase(0);
  VERIFY( umm0.count(0) == 0 );

  unordered_multimap<int, int> umm1(umm0);
  VERIFY( umm1.count(0) == 0 );
  VERIFY( umm1.count(1) == 2 );
  VERIFY( umm1.count(2) == 0 );
  VERIFY( umm1.count(3) == 3 );

  umm0.clear();
  VERIFY( umm0.count(0) == 0 );
  VERIFY( umm0.count(1) == 0 );
  VERIFY( umm0.count(2) == 0 );
  VERIFY( umm0.count(3) == 0 );

  umm1.insert(value_type(4, 1));
  umm1.insert(value_type(5, 1));
  umm1.insert(value_type(5, 2));
  umm1.insert(value_type(5, 3));
  umm1.insert(value_type(5, 4));
  VERIFY( umm1.count(4) == 1 );
  VERIFY( umm1.count(5) == 4 );

  umm1.erase(1);
  VERIFY( umm1.count(1) == 0 );

  umm1.erase(umm1.find(5));
  VERIFY( umm1.count(5) == 3 );

  umm1.insert(value_type(1, 1));
  umm1.insert(value_type(1, 2));
  VERIFY( umm1.count(1) == 2 );

  umm1.erase(5);
  VERIFY( umm1.count(5) == 0 );

  umm1.erase(umm1.find(4));
  VERIFY( umm1.count(4) == 0 );

  umm1.clear();
  VERIFY( umm1.count(0) == 0 );
  VERIFY( umm1.count(1) == 0 );
  VERIFY( umm1.count(2) == 0 );
  VERIFY( umm1.count(3) == 0 );
  VERIFY( umm1.count(4) == 0 );
  VERIFY( umm1.count(5) == 0 );
}

int main()
{
  test01();
  return 0;
}
