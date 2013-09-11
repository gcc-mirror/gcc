// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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

// { dg-options "-DTEST_DEPTH=10" { target simulator } }

// 25 algorithms, search_n

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#ifndef TEST_DEPTH
#define TEST_DEPTH 14
#endif

int array1[11] = {0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0};
int array2[TEST_DEPTH];

int pred_count;
bool 
pred(int i, int j)
{
  ++pred_count;
  return i == j;
}

bool
lexstep(int* start, int length) 
{
  int i = 0;
  int carry = 1;
  while(i < length && carry) 
    {
      if(start[i] == 1)
	start[i] = 0;
      else 
	{
	  start[i] = 1;
	  carry = 0;
	}
      i++;
    }
  return !carry;
}

int main() 
{
  using __gnu_test::test_container;
  using __gnu_test::random_access_iterator_wrapper;
  using __gnu_test::bidirectional_iterator_wrapper;
  using __gnu_test::forward_iterator_wrapper;
  
  using std::search_n;

  test_container<int,forward_iterator_wrapper> con(array1,array1 + 10);
  VERIFY(search_n(con.end(), con.end(), 0, 1) == con.end());
  VERIFY(search_n(con.end(), con.end(), 1, 1) == con.end());
  VERIFY(search_n(con.begin(), con.end(), 1, 1).ptr == array1 + 1);
  VERIFY(search_n(con.begin(), con.end(), 2, 1).ptr == array1 + 4);
  VERIFY(search_n(con.begin(), con.end(), 3, 1).ptr == array1 + 7);
  VERIFY(search_n(con.begin(), con.end(), 3, 0) == con.end());

  // Now do a brute-force comparison of the different types
  for(int i = 0; i < TEST_DEPTH; i++) 
    {
      for(int j = 0; j < i; j++)
	array2[i] = 0;
      do {
	for(int j = 0; j < i; j++)
	  {
	    test_container<int, forward_iterator_wrapper>
	      forwardcon(array2, array2 + i);
	    test_container<int, random_access_iterator_wrapper>
	      randomcon(array2, array2 + i);
	    test_container<int, bidirectional_iterator_wrapper>
	      bidircon(array2, array2 + i);

	    int* t1 = search_n(forwardcon.begin(),
			       forwardcon.end(), j, 1).ptr;
	    pred_count = 0;
	    int* t2 = search_n(forwardcon.begin(),
			       forwardcon.end(), j, 1, pred).ptr;
	    VERIFY(pred_count <= i);
	    int* t3 = search_n(bidircon.begin(),
			       bidircon.end(), j, 1).ptr;
	    pred_count = 0;
	    int* t4 = search_n(bidircon.begin(),
			       bidircon.end(), j, 1, pred).ptr;
	    VERIFY(pred_count <= i);
	    int* t5 = search_n(randomcon.begin(),
			       randomcon.end(), j, 1).ptr;
	    pred_count = 0;
	    int* t6 = search_n(randomcon.begin(),
			       randomcon.end(), j, 1, pred).ptr;
	    VERIFY(pred_count <= i);
	    VERIFY((t1 == t2) && (t2 == t3) && (t3 == t4) &&
		   (t4 == t5) && (t5 == t6));
	  }
      } 
      while(lexstep(array2, i));
    }
  return 0;
}
