// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

#include <map>
#include <testsuite_hooks.h>

using namespace std;

void test01()
{
  typedef multimap<int,double> Container;
  typedef Container::iterator iterator;
  typedef pair<iterator,iterator> itpair;
  Container m({ { 1, 1.0 }, { 1, 2.0 }, { 1, 237.0 } });
  VERIFY(m.size() == 3);
  itpair ip = m.equal_range(1);
  VERIFY(distance(ip.first, ip.second) == 3);
  iterator i = ip.first;
  VERIFY((*i++).second == 1.0);
  VERIFY((*i++).second == 2.0);
  VERIFY((*i++).second == 237.0);

  m = { {5, 55.0}, { 5, 66.0 }, { 42, 4242.0 } };
  VERIFY(m.size() == 3);
  ip = m.equal_range(5);
  VERIFY(distance(ip.first, ip.second) == 2);
  i = ip.first;
  VERIFY((*i++).second == 55.0);
  VERIFY((*i++).second == 66.0);

  m.insert({ { 7, 77.0 }, { 7, 88.0 } });
  VERIFY(m.size() == 5);
  VERIFY(m.count(5) == 2);
  VERIFY(m.count(42) == 1);
  VERIFY(m.count(7) == 2);
}

int main()
{
  __gnu_test::set_memory_limits();
  test01();
}
