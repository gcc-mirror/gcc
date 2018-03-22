// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 25.3.1.2 [lib.stable.sort]

#include <algorithm>
#include <testsuite_new_operators.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using std::stable_sort;

typedef test_container<int, random_access_iterator_wrapper> Container;

void 
test1()
{
  int array[] = { 0 };
  Container con(array, array);
  stable_sort(con.begin(), con.end());
}

void 
test2()
{
  int array[] = { 6, 5, 4, 3, 2, 1, 0 };
  Container con(array, array + 7);
  stable_sort(con.begin(), con.end());
  VERIFY(array[0] == 0 && array[1] == 1 && array[2] == 2 &&
	 array[3] == 3 && array[4] == 4 && array[5] == 5 &&
	 array[6] == 6);
}

struct S
{
  int i;
  int j;
  S() {}
  S(int in)
  {
    if(in > 0)
    {
      i = in;
      j = 1;
    }
    else
    {
      i = -in;
      j = 0;
    }
  }
};

bool 
operator<(const S& s1, const S& s2)
{ return s1.i < s2.i; }

void 
test3()
{
  S array[] = { -1, -2, 1, 2, -3 ,-5 ,3 , -4, 5, 4 };
  test_container<S, random_access_iterator_wrapper> con(array,array + 10);
  stable_sort(con.begin(), con.end());
  for(int i = 0; i < 10; ++i)
    VERIFY(array[i].j == i % 2);
}

int 
main()
{
  test1();
  test2();

  test3();

  __gnu_test::set_new_limit(sizeof(S) * 5);
  test3();

  __gnu_test::set_new_limit(sizeof(S));
  test3();

  __gnu_test::set_new_limit(0);
  test3();
}
