// 2011-10-28  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

#include <set>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  multiset<int> ms0;
  VERIFY( ms0.count(0) == 0 );
  VERIFY( ms0.count(1) == 0 );

  ms0.insert(1);
  VERIFY( ms0.count(0) == 0 );
  VERIFY( ms0.count(1) == 1 );

  ms0.insert(1);
  VERIFY( ms0.count(0) == 0 );
  VERIFY( ms0.count(1) == 2 );

  ms0.insert(2);
  VERIFY( ms0.count(2) == 1 );

  ms0.insert(3);
  ms0.insert(3);
  ms0.insert(3);
  VERIFY( ms0.count(3) == 3 );

  ms0.erase(2);
  VERIFY( ms0.count(2) == 0 );

  ms0.erase(0);
  VERIFY( ms0.count(0) == 0 );

  multiset<int> ms1(ms0);
  VERIFY( ms1.count(0) == 0 );
  VERIFY( ms1.count(1) == 2 );
  VERIFY( ms1.count(2) == 0 );
  VERIFY( ms1.count(3) == 3 );

  ms0.clear();
  VERIFY( ms0.count(0) == 0 );
  VERIFY( ms0.count(1) == 0 );
  VERIFY( ms0.count(2) == 0 );
  VERIFY( ms0.count(3) == 0 );

  ms1.insert(4);
  ms1.insert(5);
  ms1.insert(5);
  ms1.insert(5);
  ms1.insert(5);
  VERIFY( ms1.count(4) == 1 );
  VERIFY( ms1.count(5) == 4 );

  ms1.erase(1);
  VERIFY( ms1.count(1) == 0 );

  ms1.erase(ms1.find(5));
  VERIFY( ms1.count(5) == 3 );

  ms1.insert(1);
  ms1.insert(1);
  VERIFY( ms1.count(1) == 2 );

  ms1.erase(5);
  VERIFY( ms1.count(5) == 0 );

  ms1.erase(ms1.find(4));
  VERIFY( ms1.count(4) == 0 );

  ms1.clear();
  VERIFY( ms1.count(0) == 0 );
  VERIFY( ms1.count(1) == 0 );
  VERIFY( ms1.count(2) == 0 );
  VERIFY( ms1.count(3) == 0 );
  VERIFY( ms1.count(4) == 0 );
  VERIFY( ms1.count(5) == 0 );
}

int main()
{
  test01();
  return 0;
}
