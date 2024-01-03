// { dg-do run { target c++11 } }

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::minmax_element;

typedef test_container<int, forward_iterator_wrapper> Container;
typedef std::pair<forward_iterator_wrapper<int>, forward_iterator_wrapper<int> > pair_type;

void
test1()
{
  int array[] = {0};
  Container con(array, array);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array );
  VERIFY( p1.second.ptr == array );
}

void
test2()
{
  int array[] = {0};
  Container con(array, array + 1);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array );
  VERIFY( p1.second.ptr == array );
}

void
test3()
{
  int array[] = {0, 3};
  Container con(array, array + 2);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array );
  VERIFY( p1.second.ptr == array + 1 );
}

void
test4()
{
  int array[] = {3, 0};
  Container con(array, array + 2);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array + 1 );
  VERIFY( p1.second.ptr == array );
}

void
test5()
{
  int array[] = {3, 3};
  Container con(array, array + 2);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array );
  VERIFY( p1.second.ptr == array + 1 );
}

void
test6()
{
  int array[] = {6, 3, 0, 2, 6, 4, 0};
  Container con(array, array + 7);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array + 2 );
  VERIFY( p1.second.ptr == array + 4 );
}

void
test7()
{
  int array[] = {4, 4, 4, 6, 6, 6, 1, 1, 0, 0, 0, 2, 2};
  Container con(array, array + 13);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array + 8 );
  VERIFY( p1.second.ptr == array + 5 );
}

void
test8()
{
  int array[] = {1, 7, 5, 5, 10, 1, 0, 0, 8, 4, 4, 0, 10, 10, 10, 1};
  Container con(array, array + 16);
  pair_type p1 = minmax_element(con.begin(), con.end());
  VERIFY( p1.first.ptr == array + 6 );
  VERIFY( p1.second.ptr == array + 14 );
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  return 0;
}
