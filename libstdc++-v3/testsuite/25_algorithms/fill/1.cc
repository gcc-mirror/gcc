// 2004-06-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2016 Free Software Foundation, Inc.
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

// 25.2.5 [lib.alg.fill] Fill

#include <list>
#include <algorithm>
#include <testsuite_hooks.h>

class num
{
  int stored;

public:
  num(int init = 0)
  : stored(init)
  { }

  operator int() const
  { return stored; }
};

// fill
void test01()
{
  using namespace std;

  const int val = 1;

  const int V[] = { val, val, val, val, val, val, val };
  const list<int>::size_type N = sizeof(V) / sizeof(int);

  list<int> coll(N);
  fill(coll.begin(), coll.end(), val);
  VERIFY( equal(coll.begin(), coll.end(), V) );

  list<num> coll2(N);
  fill(coll2.begin(), coll2.end(), val);
  VERIFY( equal(coll2.begin(), coll2.end(), V) );
}

int main()
{
  test01();
  return 0;
}
