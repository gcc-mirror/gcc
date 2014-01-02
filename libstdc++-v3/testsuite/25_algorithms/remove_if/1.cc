// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 25.2.4 remove

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

typedef test_container<int, forward_iterator_wrapper> Container; 

bool equal1(int val) { return val == 1; }
bool equal0(int val) { return val == 0; }

void
test1()
{
  int array[1];
  Container con(array, array);
  VERIFY(std::remove_if(con.begin(), con.end(), equal1).ptr == array);
}

void
test2()
{
  int array[] = {1};
  Container con(array, array + 1);
  VERIFY(std::remove_if(con.begin(), con.end(), equal0).ptr == array + 1);
  VERIFY(std::remove_if(con.begin(), con.end(), equal1).ptr == array);
}

void
test3()
{
  int array[] = {0, 1, 0, 1, 0, 0, 1, 1};
  Container con(array, array + 8);
  VERIFY(std::remove_if(con.begin(), con.end(), equal1).ptr == array + 4);
  VERIFY(array[0] == 0 && array[1] == 0 && array[2] == 0 &&
         array[3] == 0);
}

int
main()
{
  test1();
  test2();
  test3();
}
