// { dg-do run { target c++11 } }

// 2011-10-28  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  unordered_multiset<int> ums0;
  VERIFY( ums0.count(0) == 0 );
  VERIFY( ums0.count(1) == 0 );

  ums0.insert(1);
  VERIFY( ums0.count(0) == 0 );
  VERIFY( ums0.count(1) == 1 );

  ums0.insert(1);
  VERIFY( ums0.count(0) == 0 );
  VERIFY( ums0.count(1) == 2 );

  ums0.insert(2);
  VERIFY( ums0.count(2) == 1 );

  ums0.insert(3);
  ums0.insert(3);
  ums0.insert(3);
  VERIFY( ums0.count(3) == 3 );

  ums0.erase(2);
  VERIFY( ums0.count(2) == 0 );

  ums0.erase(0);
  VERIFY( ums0.count(0) == 0 );

  unordered_multiset<int> ums1(ums0);
  VERIFY( ums1.count(0) == 0 );
  VERIFY( ums1.count(1) == 2 );
  VERIFY( ums1.count(2) == 0 );
  VERIFY( ums1.count(3) == 3 );

  ums0.clear();
  VERIFY( ums0.count(0) == 0 );
  VERIFY( ums0.count(1) == 0 );
  VERIFY( ums0.count(2) == 0 );
  VERIFY( ums0.count(3) == 0 );

  ums1.insert(4);
  ums1.insert(5);
  ums1.insert(5);
  ums1.insert(5);
  ums1.insert(5);
  VERIFY( ums1.count(4) == 1 );
  VERIFY( ums1.count(5) == 4 );

  ums1.erase(1);
  VERIFY( ums1.count(1) == 0 );

  ums1.erase(ums1.find(5));
  VERIFY( ums1.count(5) == 3 );

  ums1.insert(1);
  ums1.insert(1);
  VERIFY( ums1.count(1) == 2 );

  ums1.erase(5);
  VERIFY( ums1.count(5) == 0 );

  ums1.erase(ums1.find(4));
  VERIFY( ums1.count(4) == 0 );

  ums1.clear();
  VERIFY( ums1.count(0) == 0 );
  VERIFY( ums1.count(1) == 0 );
  VERIFY( ums1.count(2) == 0 );
  VERIFY( ums1.count(3) == 0 );
  VERIFY( ums1.count(4) == 0 );
  VERIFY( ums1.count(5) == 0 );
}

int main()
{
  test01();
  return 0;
}
