// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

// 25.2.11 [alg.equal]

// { dg-do run { target c++14 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, input_iterator_wrapper> Container;
typedef test_container<int, random_access_iterator_wrapper> RA_Container;
int array1[] = {0, 1};
int array2[] = {1, 0};
int array3[] = {1, 0};

struct equal_to
{
  static int count;

  bool operator()(int l, int r)
  {
    ++count;
    return l == r;
  }
} eq;

int equal_to::count = 0;

bool __attribute__((unused)) test = false;

void test1()
{
  const Container con1(array1, array1);
  const Container con2(array2, array2);

  auto c1 = con1;
  auto c2 = con2;
  VERIFY( std::equal(c1.begin(), c1.end(), c2.begin(), c2.end()) );
  VERIFY( equal_to::count == 0 );

  c1 = con1;
  c2 = con2;
  VERIFY( std::equal(c1.begin(), c1.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 0 );
}

void test2()
{
  const Container con1(array1, array1 + 0);
  const Container con2(array2, array2 + 2);

  auto c1 = con1;
  auto c2 = con2;
  VERIFY( !std::equal(c1.begin(), c1.end(), c2.begin(), c2.end()) );

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c1.begin(), c1.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 0 );

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c1.begin(), c1.end()) );

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c1.begin(), c1.end(), eq) );
  VERIFY( equal_to::count == 0 );
}

void test3()
{
  const Container con1(array1, array1 + 2);
  const Container con2(array2, array2 + 2);

  auto c1 = con1;
  auto c2 = con2;
  VERIFY( !std::equal(c1.begin(), c1.end(), c2.begin(), c2.end()) );

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c1.begin(), c1.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 1 );
  equal_to::count = 0;

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c1.begin(), c1.end()) );

  c1 = con1;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c1.begin(), c1.end(), eq) );
  VERIFY( equal_to::count == 1 );
  equal_to::count = 0;
}

void test4()
{
  const Container con3(array3, array3 + 2);
  const Container con2(array2, array2 + 2);

  auto c3 = con3;
  auto c2 = con2;
  VERIFY( std::equal(c3.begin(), c3.end(), c2.begin(), c2.end()) );

  c3 = con3;
  c2 = con2;
  VERIFY( std::equal(c3.begin(), c3.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 2 );
  equal_to::count = 0;

  c3 = con3;
  c2 = con2;
  VERIFY( std::equal(c2.begin(), c2.end(), c3.begin(), c3.end()) );

  c3 = con3;
  c2 = con2;
  VERIFY( std::equal(c2.begin(), c2.end(), c3.begin(), c3.end(), eq) );
  VERIFY( equal_to::count == 2 );
  equal_to::count = 0;
}

void test5()
{
  const Container con3(array3, array3 + 1);
  const Container con2(array2, array2 + 2);

  auto c3 = con3;
  auto c2 = con2;
  VERIFY( !std::equal(c3.begin(), c3.end(), c2.begin(), c2.end()) );

  c3 = con3;
  c2 = con2;
  VERIFY( !std::equal(c3.begin(), c3.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 1 );
  equal_to::count = 0;

  c3 = con3;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c3.begin(), c3.end()) );

  c3 = con3;
  c2 = con2;
  VERIFY( !std::equal(c2.begin(), c2.end(), c3.begin(), c3.end(), eq) );
  VERIFY( equal_to::count == 1 );
  equal_to::count = 0;
}

void test6()
{
  RA_Container c3(array3, array3 + 1);
  RA_Container c2(array2, array2 + 2);

  VERIFY( !std::equal(c3.begin(), c3.end(), c2.begin(), c2.end()) );

  VERIFY( !std::equal(c3.begin(), c3.end(), c2.begin(), c2.end(), eq) );
  VERIFY( equal_to::count == 0 );

  VERIFY( !std::equal(c2.begin(), c2.end(), c3.begin(), c3.end()) );

  VERIFY( !std::equal(c2.begin(), c2.end(), c3.begin(), c3.end(), eq) );
  VERIFY( equal_to::count == 0 );
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
}
