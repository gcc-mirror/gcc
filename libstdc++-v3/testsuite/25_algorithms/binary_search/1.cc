// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 25.3.3.4 [lib.binary.search]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::binary_search;

typedef test_container<int, forward_iterator_wrapper> Container;

void 
test1()
{
  int array[] = {0};
  Container con(array, array);
  VERIFY(!binary_search(con.begin(), con.end(), 1));
}

void
test2()
{
  int array[] = {0, 2, 4, 6, 8};
  Container con(array, array + 5);
  for(int i = 0; i < 10; i += 2)
    VERIFY(binary_search(con.begin(), con.end(), i));
  for(int i = -1; i < 11; i += 2)
    VERIFY(!binary_search(con.begin(), con.end(), i));
}

int main()
{
  test1();
  test2();
}
