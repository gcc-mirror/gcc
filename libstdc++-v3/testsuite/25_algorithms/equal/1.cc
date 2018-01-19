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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Container;
int array1[] = {0, 1};
int array2[] = {1, 0};
int array3[] = {1, 0};

bool __attribute__((unused)) test = false;

void test1()
{
  Container con1(array1, array1);
  Container con2(array2, array2);
  VERIFY( std::equal(con1.begin(), con1.end(), con2.begin()) );
}

void test2()
{
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( !std::equal(con2.begin(), con2.end(), con1.begin()) );
}

void test3()
{
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( !std::equal(con1.begin(), con1.end(), con2.begin()) );
}

void test4()
{
  Container con3(array3, array3 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( std::equal(con3.begin(), con3.end(), con2.begin()) );
}

int main()
{
  test1();
  test2();
  test3();
  test4();
}
