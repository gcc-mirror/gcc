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
  bool test __attribute__((unused)) = true;
  using namespace std;

  unordered_set<int> us0;
  VERIFY( us0.count(0) == 0 );
  VERIFY( us0.count(1) == 0 );

  us0.insert(1);
  VERIFY( us0.count(0) == 0 );
  VERIFY( us0.count(1) == 1 );

  us0.insert(1);
  VERIFY( us0.count(0) == 0 );
  VERIFY( us0.count(1) == 1 );

  us0.insert(2);
  VERIFY( us0.count(2) == 1 );

  us0.insert(3);
  us0.insert(3);
  us0.insert(3);
  VERIFY( us0.count(3) == 1 );

  us0.erase(2);
  VERIFY( us0.count(2) == 0 );

  us0.erase(0);
  VERIFY( us0.count(0) == 0 );

  unordered_set<int> us1(us0);
  VERIFY( us1.count(0) == 0 );
  VERIFY( us1.count(1) == 1 );
  VERIFY( us1.count(2) == 0 );
  VERIFY( us1.count(3) == 1 );

  us0.clear();
  VERIFY( us0.count(0) == 0 );
  VERIFY( us0.count(1) == 0 );
  VERIFY( us0.count(2) == 0 );
  VERIFY( us0.count(3) == 0 );

  us1.insert(4);
  us1.insert(5);
  us1.insert(5);
  us1.insert(5);
  us1.insert(5);
  VERIFY( us1.count(4) == 1 );
  VERIFY( us1.count(5) == 1 );

  us1.erase(1);
  VERIFY( us1.count(1) == 0 );

  us1.erase(us1.find(5));
  VERIFY( us1.count(5) == 0 );

  us1.insert(1);
  us1.insert(1);
  VERIFY( us1.count(1) == 1 );

  us1.erase(5);
  VERIFY( us1.count(5) == 0 );

  us1.erase(us1.find(4));
  VERIFY( us1.count(4) == 0 );

  us1.clear();
  VERIFY( us1.count(0) == 0 );
  VERIFY( us1.count(1) == 0 );
  VERIFY( us1.count(2) == 0 );
  VERIFY( us1.count(3) == 0 );
  VERIFY( us1.count(4) == 0 );
  VERIFY( us1.count(5) == 0 );
}

int main()
{
  test01();
  return 0;
}
