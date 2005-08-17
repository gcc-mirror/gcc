// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.3.3.1 [lib.lower.bound]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::lower_bound;

typedef test_container<int, forward_iterator_wrapper> Container;
int array[] = {0, 0, 0, 0, 1, 1, 1, 1};

void 
test1()
{
  for(int i = 0; i < 5; ++i)
    for(int j = 4; j < 7; ++j)
      {
	Container con(array + i, array + j);
	VERIFY(lower_bound(con.begin(), con.end(), 1).ptr == array + 4);
      }
}

int 
main()
{
  test1();
}
