// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
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



void
test1()
{
  bool test __attribute__((unused)) = true;
  int data[] = {1, 2, 3, 4, 5};
  rvalstruct array[5];
  std::copy(data, data + 5, array); 
  Fcontainer fcon(array, array + 5);
  Bcontainer bcon(array, array + 5);
  Rcontainer rcon(array, array + 5);
  
  std::rotate(fcon.begin(), fcon.it(2), fcon.end());
  VERIFY(array[0].val == 3 && array[1].val == 4 && array[2].val == 5 && 
	 array[3].val == 1 && array[4].val == 2);
  for(int i=0;i<5;i++)
    VERIFY(array[i].valid == true);

  std::rotate(bcon.begin(), bcon.it(2), bcon.end());
  VERIFY(array[0].val == 5 && array[1].val == 1 && array[2].val == 2 && 
	 array[3].val == 3 && array[4].val == 4);
  for(int i=0;i<5;i++)
    VERIFY(array[i].valid);

  std::rotate(rcon.begin(), rcon.it(2), rcon.end());
  VERIFY(array[0].val == 2 && array[1].val == 3 && array[2].val == 4 && 
	 array[3].val == 5 && array[4].val == 1);
  for(int i=0;i<5;i++)
    VERIFY(array[i].valid); 
}

int
main()
{
  test1();
}
