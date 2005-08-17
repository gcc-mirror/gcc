// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 25.1.5 [lib.alg.search]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using std::search;

typedef test_container<int, forward_iterator_wrapper> Container;
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

int 
main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
}
