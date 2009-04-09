// Copyright (C) 2005, 2009 Free Software Foundation, Inc.
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

// 25.1.5 [lib.alg.search]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;
using std::search;

typedef test_container<int, forward_iterator_wrapper> Container;
typedef test_container<int, random_access_iterator_wrapper> RAcontainer;
int array1[] = {0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1};
int array2[] = {0, 0, 0};

void 
test1()
{
  bool test __attribute__((unused)) = true;
  Container con1(array1, array1);
  Container con2(array1, array1 + 1);
  VERIFY(search(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr == array1);
  VERIFY(search(con2.begin(), con2.end(), con1.begin(), con1.end()).ptr == array1);
}

void
test2()
{
  bool test __attribute__((unused)) = true;
  Container con1(array1, array1 + 3);
  Container con2(array2, array2 + 3);
  VERIFY(search(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr 
         == array1);
}

void
test3()
{
  bool test __attribute__((unused)) = true;
  Container con1(array1 + 3, array1 + 10);
  Container con2(array2, array2 + 3);
  VERIFY(search(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr 
         == array1 + 10);
}

void
test4()
{
  bool test __attribute__((unused)) = true;
  Container con1(array1, array1 + 10);
  Container con2(array2, array2 + 1);
  VERIFY(search(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr
	 == array1);
}

void
test5()
{
  bool test __attribute__((unused)) = true;
  Container con1(array1 + 6, array1 + 10);
  Container con2(array2, array2 + 1);
  VERIFY(search(con1.begin(), con1.end(), con2.begin(), con2.end()).ptr
	 == array1 + 10);
}

void
test6()
{
  bool test __attribute__((unused)) = true;
  int array3[]={2, 2, 1, 2, 3, 5};
  int array4[]={1, 2, 3, 4};
  Container con1(array3, array3 + 3);
  Container con2(array3, array3 + 4);
  Container con3(array3, array3 + 5);
  Container con4(array3, array3 + 6);
  Container endcon(array4, array4 + 4);
  VERIFY(search(con1.begin(), con1.end(), endcon.begin(), endcon.end()).ptr
	 == array3 + 3);
  VERIFY(search(con2.begin(), con2.end(), endcon.begin(), endcon.end()).ptr
	 == array3 + 4);
  VERIFY(search(con3.begin(), con3.end(), endcon.begin(), endcon.end()).ptr
	 == array3 + 5);
  VERIFY(search(con4.begin(), con4.end(), endcon.begin(), endcon.end()).ptr
	 == array3 + 6);
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

void test7()
{
  int array1[6];
  int array2[6];
  for(int length1 = 0; length1 < 6; length1++)
  {
    for(int length2 = 0; length2 < 6; length2++)
    {
      std::fill_n(array1, length1, 0);
      while(lexstep(array1, length1))
      {
 	std::fill_n(array2, length2, 0);
 	while(lexstep(array2, length2))
        {
          Container con1(array1, array1 + length1);
          Container con2(array2, array2 + length2);
          RAcontainer rcon1(array1, array1 + length1);
          RAcontainer rcon2(array2, array2 + length2);
          VERIFY(search(con1.begin(), con1.end(), con2.begin(), 
 			con2.end()).ptr ==
                 search(rcon1.begin(), rcon1.end(), rcon2.begin(),
                 rcon2.end()).ptr);
        }
      } 
    }
  }
}

int 
main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
}
