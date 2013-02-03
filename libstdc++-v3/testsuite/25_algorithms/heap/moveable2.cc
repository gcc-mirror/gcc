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

// { dg-options "-std=gnu++0x -DITERATIONS=5" { target simulator } }

// 25.3.6 Heap operations [lib.alg.heap.operations]

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

#ifndef ITERATIONS
#define ITERATIONS 9
#endif

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, random_access_iterator_wrapper> container;
typedef test_container<int, random_access_iterator_wrapper> container_ref;

bool are_ordered(const rvalstruct& lhs, const rvalstruct& rhs)
{ return lhs < rhs; }

bool are_ordered_int(const int& lhs, const int& rhs)
{ return lhs < rhs; }

void 
check_make(int* array, int length)
{
  bool test __attribute__((unused)) = true;

  rvalstruct makeheap[9];
  int        makeheap_ref[9];
  std::copy(array, array + length, makeheap);
  std::copy(array, array + length, makeheap_ref);  
  container makecon(makeheap, makeheap + length);
  container_ref makecon_ref(makeheap_ref, makeheap_ref + length);
  std::make_heap(makecon.begin(), makecon.end(), are_ordered);
  std::make_heap(makecon_ref.begin(), makecon_ref.end(), are_ordered_int);
  for (int z = 0; z < length; ++z)
    VERIFY( makeheap[z] == makeheap_ref[z] );
  VERIFY( std::__is_heap(makecon.begin(), makecon.end(), are_ordered) );
  for (int z = 0; z < length; ++z)
    VERIFY( makeheap[z].valid );
}

void
check_pop(int* array, int length)
{
  bool test __attribute__((unused)) = true;

  rvalstruct popheap[9];
  int        popheap_ref[9];
  std::copy(array, array + length, popheap);
  std::copy(array, array + length, popheap_ref);
  container popcon(popheap, popheap + length);
  container_ref popcon_ref(popheap_ref, popheap_ref + length);
  std::pop_heap(popcon.begin(), popcon.end(), are_ordered);
  std::pop_heap(popcon_ref.begin(), popcon_ref.end(), are_ordered_int);
  for (int z = 0; z < length; ++z)
    VERIFY( popheap[z] == popheap_ref[z] );
  VERIFY( (std::__is_heap(popheap, popheap + length - 1), are_ordered) );
  for (int z = 0; z < length; ++z)
    VERIFY( popheap[z].val <= popheap[length-1].val && popheap[z].valid );
}

void
check_sort(int* array, int length)
{
  bool test __attribute__((unused)) = true;

  rvalstruct sortheap[9];
  int        sortheap_ref[9];
  std::copy(array, array + length, sortheap);
  std::copy(array, array + length, sortheap_ref);
  container sortcon(sortheap, sortheap + length);
  container_ref sortcon_ref(sortheap_ref, sortheap_ref + length);
  std::sort_heap(sortcon.begin(), sortcon.end(), are_ordered);
  std::sort_heap(sortcon_ref.begin(), sortcon_ref.end(), are_ordered_int);
  for (int z = 0; z < length; ++z)
    VERIFY( sortheap[z] == sortheap_ref[z] );
  for (int z = 0; z < length - 1; ++z)
    VERIFY( sortheap[z].val <= sortheap[z + 1].val && sortheap[z].valid );
  VERIFY( sortheap[length - 1].valid );
}

void
check_push(int* array, int pushval, int length)
{
  bool test __attribute__((unused)) = true;

  rvalstruct pushheap[10];
  int        pushheap_ref[10];
  std::copy(array, array + length, pushheap);
  std::copy(array, array + length, pushheap_ref);  
  pushheap[length] = pushval;
  pushheap_ref[length] = pushval;
  container pushcon(pushheap, pushheap + length + 1);
  container_ref pushcon_ref(pushheap_ref, pushheap_ref + length + 1);
  std::push_heap(pushcon.begin(), pushcon.end(), are_ordered);
  std::push_heap(pushcon_ref.begin(), pushcon_ref.end(), are_ordered_int);
  for (int z = 0; z < length + 1; ++z)
    VERIFY( pushheap[z] == pushheap_ref[z] );
  VERIFY( std::__is_heap(pushheap, pushheap + length + 1) );
  for (int z = 0; z < length + 1; ++z)
    VERIFY( pushheap[z].valid );
}

void
test01()
{
  int array[9];
  for (int i = 1; i < ITERATIONS; ++i)
    {
      for(int z = 0; z < i; ++z)
	array[z] = z;
      while (std::next_permutation(array, array + i))
	{
	  check_make(array, i);
	  if (std::__is_heap(array, array + i, are_ordered_int))
	    {
	      check_pop(array, i);
	      check_sort(array, i);
	      for (int pushval = -1; pushval <= i; ++pushval)
		check_push(array, pushval, i);
	    }
	}
    }
}

int
main()
{
  test01();
  return 0;
}
