// { dg-options "-std=gnu++0x" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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

// 25.3.4 [lib.alg.merge]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, bidirectional_iterator_wrapper> container;

void
test01()
{
  bool test __attribute__((unused)) = true;

  int array[]={0,2,4,1,3,5};
  rvalstruct rv_array[6];
  std::copy(array, array + 6, rv_array);
  container con(rv_array, rv_array + 6);
  std::inplace_merge(con.begin(), con.it(3), con.end());
  VERIFY( rv_array[0] == 0 && rv_array[1] == 1 && rv_array[2] == 2
	  && rv_array[3] == 3 && rv_array[4] == 4 && rv_array[5] == 5 );
}

int 
main()
{
  test01();
  return 0;
}
