// { dg-do run { target c++11 } }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

// 25.2.10 rotate

// Tests rotate when an moveable class is used

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, forward_iterator_wrapper> Fcontainer; 
typedef test_container<rvalstruct, bidirectional_iterator_wrapper> Bcontainer; 
typedef test_container<rvalstruct, random_access_iterator_wrapper> Rcontainer; 

template<typename Con>
  void
  test_con(int length, int rotate_pos)
  {
    /* Make sure the VLA upper bound is positive. */
    rvalstruct array[length + 1];
    for(int i = 0; i < length; ++i)
      array[i] = i;
    Con con(array, array + length);
    std::rotate(con.begin(), con.it(rotate_pos), con.end());

    if(length != 0)
      {
	for(int i = 0; i < length; ++i)
	  VERIFY( array[i].valid && array[i].val == (i + rotate_pos) % length );
      }
  }

void
test01()
{
  for(int i = 0; i < 20; ++i)
    {
      for(int j = 0; j <= i; ++j)
	{
	  test_con<Fcontainer>(i, j);
	  test_con<Bcontainer>(i, j);
	  test_con<Rcontainer>(i, j);
	}
    }
}

int
main()
{
  test01();
  return 0;
}
