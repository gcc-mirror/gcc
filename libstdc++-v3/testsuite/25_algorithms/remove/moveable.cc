// { dg-do run { target c++11 } }

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

// 25.2.4 remove

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, forward_iterator_wrapper> Container; 

void
test1()
{
  int intarray[] = {1};
  rvalstruct array[1];
  std::copy(intarray, intarray + 1, array);
  Container con(array, array + 1);
  rvalstruct remove_val0(0);
  rvalstruct remove_val1(1);
  VERIFY(std::remove(con.begin(), con.end(), remove_val0).ptr == array + 1);
  VERIFY(std::remove(con.begin(), con.end(), remove_val1).ptr == array);
}

void
test2()
{
  int intarray[] = {0, 1, 0, 1, 0, 0, 1, 1};
  rvalstruct array[8];
  std::copy(intarray, intarray + 8, array);
  Container con(array, array + 8);
  rvalstruct remove_val(1);
  VERIFY(std::remove(con.begin(), con.end(), remove_val).ptr == array + 4);
  VERIFY(array[0].val == 0 && array[1].val == 0 && array[2].val == 0 && 
	 array[3].val == 0);
}

int
main()
{
  test1();
  test2();
}
