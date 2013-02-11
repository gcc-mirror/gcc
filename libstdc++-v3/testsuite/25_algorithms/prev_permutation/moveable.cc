// { dg-options "-std=gnu++0x" }

// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

// 25.3.9 [lib.alg.permutation.generators]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;
using std::prev_permutation;

typedef test_container<rvalstruct, bidirectional_iterator_wrapper> Container;

void
test1()
{
  bool test __attribute__((unused)) = true;

  // Note: The standard is unclear on what should happen in this case.
  // This seems the only really sensible behaviour, and what is done.
  rvalstruct array[] = {0};
  Container con(array, array);
  VERIFY( !prev_permutation(con.begin(), con.end()) );
}

void
test2()
{
  bool test __attribute__((unused)) = true;

  rvalstruct array[] = {0};
  Container con(array, array + 1);
  VERIFY( !prev_permutation(con.begin(), con.end()) );
}

void
test3()
{
  bool test __attribute__((unused)) = true;

  rvalstruct array[] = {3, 0};
  Container con(array, array + 2);
  VERIFY( prev_permutation(con.begin(), con.end()) );
  VERIFY( array[0] == 0 && array[1] == 3 );
  VERIFY( !prev_permutation(con.begin(), con.end()) );
  VERIFY( array[0] == 3 && array[1] == 0 );
}

void
test4()
{
  bool test __attribute__((unused)) = true;

  int array[6] = {5, 4, 3, 2, 1, 0};
  for(int i = 0 ; i < 719; ++i)
    {
      rvalstruct temp_array[6];
      std::copy(array, array + 6, temp_array);
      Container con(temp_array, temp_array + 6);
      VERIFY( prev_permutation(array, array + 6) );

// XXX FIXME:  parallel-mode should deal correctly with moveable-only types
// per C++0x, at minimum smoothly fall back to serial.
#ifndef _GLIBCXX_PARALLEL
      VERIFY( !std::lexicographical_compare(temp_array, temp_array + 6, 
					    array, array + 6) );
#endif
    }
  VERIFY( !prev_permutation(array,array + 6)) ;
  for(int i = 0; i < 6; ++i)
    VERIFY( array[i] == (5 - i) );
}

bool
are_ordered(const rvalstruct& lhs, const rvalstruct& rhs)
{ return lhs < rhs; }

void
test5()
{
  bool test __attribute__((unused)) = true;

  int array[6] = {5, 4, 3, 2, 1, 0};
  for(int i = 0 ; i < 719; ++i)
    {
      rvalstruct temp_array[6];
      std::copy(array, array + 6, temp_array);
      Container con(temp_array, temp_array + 6);
      VERIFY( prev_permutation(array, array + 6, are_ordered) );

// XXX FIXME:  parallel-mode should deal correctly with moveable-only types
// per C++0x, at minimum smoothly fall back to serial.
#ifndef _GLIBCXX_PARALLEL
      VERIFY( !std::lexicographical_compare(temp_array, temp_array + 6, 
					    array, array + 6, are_ordered) );
#endif
    }
  VERIFY( !prev_permutation(array,array + 6, are_ordered) );
  for(int i = 0; i < 6; ++i)
    VERIFY( array[i] == (5 - i) );
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  return 0;
}
