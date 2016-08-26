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

#include <map>
#include <testsuite_hooks.h>

using namespace std;

int test01()
{
  bool test __attribute__((unused)) = true;

  map<int,double> m({ { 1, 1.0 }, { 2, 2.0 }, { 42, 237.0 } });
  VERIFY(m.size() == 3);
  VERIFY(m[1] == 1.0);
  VERIFY(m[2] == 2.0);
  VERIFY(m[42] == 237.0);

  m = { {5, 55.0}, { 6, 66.0 } };
  VERIFY(m.size() == 2);
  VERIFY(m[5] == 55.0);
  VERIFY(m[6] == 66.0);

  m.insert({ { 7, 77.0 }, { 8, 88.0 } });
  VERIFY(m.size() == 4);
  VERIFY(m[5] == 55.0);
  VERIFY(m[6] == 66.0);
  VERIFY(m[7] == 77.0);
  VERIFY(m[8] == 88.0);
  return test;
}

int main()
{
  __gnu_test::set_memory_limits();
  test01();
}
