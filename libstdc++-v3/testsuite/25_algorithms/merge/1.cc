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

// 25.3.4 [lib.alg.merge]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using std::merge;

typedef test_container<int, input_iterator_wrapper> Icontainer;
typedef test_container<int, output_iterator_wrapper> Ocontainer;

void 
test1()
{
  int array1[1], array2[1];
  Icontainer con1(array1, array1);
  Icontainer con2(array1, array1);
  Ocontainer con3(array2, array2);
  VERIFY(merge(con1.begin(), con1.end(), con2.begin(), con2.end(),
	       con3.begin()).ptr == array2);
}

void 
test2()
{
  int array1[]={0,1,4};
  int array2[]={2,3};
  int array3[5];
  Icontainer con1(array1, array1 + 3);
  Icontainer con2(array2, array2 + 2);
  Ocontainer con3(array3, array3 + 5);
  VERIFY(merge(con1.begin(), con1.end(), con2.begin(), con2.end(),
	       con3.begin()).ptr == array3 + 5);
  VERIFY(array3[0] == 0 && array3[1] == 1 && array3[2] == 2 &&
	 array3[3] == 3 && array3[4] == 4);

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
  S array1[] = { -1 , -3};
  S array2[] = { 1, 2, 3};
  S array3[5];
  merge(array1, array1 + 2, array2, array2 + 3, array3);
  VERIFY(array3[0].j == 0 && array3[1].j == 1 && array3[2].j == 1 &&
         array3[3].j == 0 && array3[4].j == 1);
}

int 
main()
{
  test1();
  test2();
  test3();
}
