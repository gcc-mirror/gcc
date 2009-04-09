// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 25.3.3.2 [lib.upper.bound]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::upper_bound;

typedef test_container<int, forward_iterator_wrapper> Container;
int array[] = {0, 0, 0, 0, 1, 1, 1, 1};

void 
test1()
{
  for(int i = 0; i < 5; ++i)
    for(int j = 4; j < 7; ++j)
      {
	Container con(array + i, array + j);
	VERIFY(upper_bound(con.begin(), con.end(), 0).ptr == array + 4);
      }
}

int 
main()
{
  test1();
}
