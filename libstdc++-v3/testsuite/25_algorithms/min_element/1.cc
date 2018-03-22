// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 25.3.7 [lib.alg.min.max]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::min_element;

typedef test_container<int, forward_iterator_wrapper> Container;

void
test1()
{
  // Note: The standard is unclear on what should happen in this case.
  // This seems the only really sensible behaviour, and what is done.
  int array[] = {0};
  Container con(array, array);
  VERIFY(min_element(con.begin(), con.end()).ptr == array);
}

void
test2()
{
  int array[] = {0};
  Container con(array, array + 1);
  VERIFY(min_element(con.begin(), con.end()).ptr == array);
}

void
test3()
{
  int array[] = {0, 3};
  Container con(array, array + 2);
  VERIFY(min_element(con.begin(), con.end()).ptr == array);
}

void
test4()
{
  int array[] = {6, 3, 0, 2, 6, 4, 0};
  Container con(array, array + 7);
  VERIFY(min_element(con.begin(), con.end()).ptr == array + 2);
}

int main()
{
  test1();
  test2();
  test3();
  test4();
}
