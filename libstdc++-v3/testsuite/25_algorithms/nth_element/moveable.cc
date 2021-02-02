// { dg-do run { target c++11 } }

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 25.3.2 [lib.alg.nth.element]

#undef _GLIBCXX_CONCEPT_CHECKS

// XXX FIXME:  parallel-mode should deal correctly with moveable-only types
// per C++0x, at minimum smoothly fall back to serial.
#undef _GLIBCXX_PARALLEL

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using std::nth_element;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, random_access_iterator_wrapper> Container;

void 
test1()
{
  int intarray[] = {6, 5, 4, 3, 2, 1, 0};
  rvalstruct array[7];
  std::copy(intarray, intarray + 7, array);
  Container con(array, array + 7);
  nth_element(con.begin(), con.it(3), con.end());
  for(int i = 0; i < 3; ++i)
    VERIFY( array[i].val < 3 );
  for(int i = 4; i < 7; ++i)
    VERIFY( array[i].val > 3 );
  for(int i = 0; i < 7; ++i)
    VERIFY( array[i].valid );
}

void 
test2()
{
  int intarray[] = {0, 6, 1, 5, 2, 4, 3};
  rvalstruct array[7];
  std::copy(intarray, intarray + 7, array);
  Container con(array,array + 7);
  nth_element(con.begin(), con.it(3), con.end());
  for(int i = 0; i < 3; ++i)
    VERIFY( array[i].val < 3 );
  for(int i = 4; i < 7; ++i)
    VERIFY( array[i].val > 3 );
  for(int i = 0; i < 7; ++i)
    VERIFY( array[i].valid );  
}

bool
are_less(const rvalstruct& lhs, const rvalstruct& rhs)
{ return lhs < rhs; }

void 
test3()
{
  int intarray[] = {0, 6, 1, 5, 2, 4, 3};
  rvalstruct array[7];
  std::copy(intarray, intarray + 7, array);
  Container con(array,array + 7);
  nth_element(con.begin(), con.it(3), con.end(), are_less);
  for(int i = 0; i < 3; ++i)
    VERIFY( array[i].val < 3 );
  for(int i = 4; i < 7; ++i)
    VERIFY( array[i].val > 3 );
  for(int i = 0; i < 7; ++i)
    VERIFY( array[i].valid );  
}

int 
main()
{
  test1();
  test2();
  test3();
  return 0;
}
