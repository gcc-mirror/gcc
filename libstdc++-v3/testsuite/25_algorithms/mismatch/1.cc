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

// 25.1.7 [lib.mismatch]

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Container;
int array1[] = {0, 1};
int array2[] = {1, 0};
int array3[] = {1, 0, 1};

bool __attribute__((unused)) test = false;

void test1a()
{
  Container con1(array1, array1);
  Container con2(array2, array2);
  VERIFY( std::mismatch(con1.begin(), con1.end(), con2.begin()).first.ptr
	  == array1 );
}

void test1b()
{
  Container con1(array1, array1);
  Container con2(array2, array2);
  VERIFY( std::mismatch(con1.begin(), con1.end(), con2.begin()).second.ptr
	  == array2 );
}

void test2a()
{
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( std::mismatch(con1.begin(), con1.end(), con2.begin()).first.ptr
	  == array1 );
}

void test2b()
{
  Container con1(array1, array1 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( std::mismatch(con1.begin(), con1.end(), con2.begin()).second.ptr
	  == array2 );
}

void test3a()
{
  Container con3(array3, array3 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( std::mismatch(con3.begin(), con3.end(), con2.begin()).first.ptr
	  == array3 + 2 );
}

void test3b()
{
  Container con3(array3, array3 + 2);
  Container con2(array2, array2 + 2);
  VERIFY( std::mismatch(con3.begin(), con3.end(), con2.begin()).second.ptr
	  == array2 + 2 );
}

int main()
{
  test1a();
  test1b();
  test2a();
  test2b();
  test3a();
  test3b();
}
