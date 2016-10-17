// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <unordered_set>
#include <testsuite_hooks.h>

using namespace std;

void test01()
{
  unordered_set<int> m({ 1, 5, 37 });
  VERIFY(m.size() == 3);
  VERIFY(m.count(1) == 1);
  VERIFY(m.count(5) == 1);
  VERIFY(m.count(37) == 1);
  VERIFY(m.count(42) == 0);

  m = { 28, 37, 102 };
  VERIFY(m.size() == 3);
  VERIFY(m.count(28) == 1);
  VERIFY(m.count(37) == 1);
  VERIFY(m.count(102) == 1);
  VERIFY(m.count(1) == 0);

  m.insert({ 42 });
  VERIFY(m.size() == 4);
  VERIFY(m.count(28) == 1);
  VERIFY(m.count(37) == 1);
  VERIFY(m.count(102) == 1);
  VERIFY(m.count(42) == 1);
  VERIFY(m.count(1) == 0);
}

int main()
{
  __gnu_test::set_memory_limits();
  test01();
}
