// { dg-do run { target c++11 } }

// 2008-06-28  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

typedef test_container<int, forward_iterator_wrapper> Container;
int array[] = {0, 0, 1, 1, 1, 1};

bool
predicate(const int& i) 
{ return i == 0; }

void
test1()
{
  Container con(array, array);

  forward_iterator_wrapper<int> mid =
    std::partition_point(con.begin(), con.end(), predicate);

  VERIFY( std::all_of(con.begin(), mid, predicate) );
  VERIFY( std::none_of(mid, con.end(), predicate) );
}

void
test2()
{
  Container con(array, array + 1);

  forward_iterator_wrapper<int> mid =
    std::partition_point(con.begin(), con.end(), predicate);

  VERIFY( std::all_of(con.begin(), mid, predicate) );
  VERIFY( std::none_of(mid, con.end(), predicate) );
}

void
test3()
{
  Container con(array, array + 6);

  forward_iterator_wrapper<int> mid =
    std::partition_point(con.begin(), con.end(), predicate);

  VERIFY( std::all_of(con.begin(), mid, predicate) );
  VERIFY( std::none_of(mid, con.end(), predicate) );
}

int 
main()
{
  test1();
  test2();
  test3();
  return 0;
}
