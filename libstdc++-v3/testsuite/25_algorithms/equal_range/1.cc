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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 25.3.3.3 [lib.equal.range]

#include <algorithm>
#include <utility>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::equal_range;

typedef test_container<int, forward_iterator_wrapper> Container;
int array[] = {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2};

void 
test1()
{
  for(int i = 0; i < 6; ++i)
    for(int j = 6; j < 12; ++j)
      {
	Container con(array + i, array + j);
        VERIFY(equal_range(con.begin(), con.end(), 1).first.ptr ==
	       array + std::max(i, 4));
        VERIFY(equal_range(con.begin(), con.end(), 1).second.ptr ==
               array + std::min(j, 8));
      }
}

void
test2()
{
  int array[]={0, 0, 2, 2, 2};
  Container con(array, array + 5);
  VERIFY(equal_range(con.begin(), con.end(), 1).first.ptr ==
	 array + 2);
  VERIFY(equal_range(con.begin(), con.end(), 1).second.ptr ==
	 array + 2);
}

int 
main()
{
  test1();
  test2();
}
