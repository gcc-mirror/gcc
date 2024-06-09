// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <array>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

// Lambda that calls lexicographical_compare_three_way on two ranges.
// Arguments are passed by value intentionally, so that a copy of the range
// is traversed and the original is not modified. Otherwise when the range
// has input iterators the range will be consumed after the first comparison.
auto lexicomp3 = [](auto r1, auto r2) {
  return std::lexicographical_compare_three_way(r1.begin(), r1.end(),
						r2.begin(), r2.end());
};

void
test01()
{
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  using __gnu_test::forward_iterator_wrapper;
  int arr1[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int arr2[] = { 0, 1, 2, 3, 4, 5, 6, 777 };

  {
    test_container<int, input_iterator_wrapper> c1(arr1);
    test_container<int, input_iterator_wrapper> c2(arr2);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c1, c2) < 0 );
    VERIFY( lexicomp3(c2, c1) > 0 );
  }

  {
    test_container<int, input_iterator_wrapper> c1(arr1, arr1+6);
    test_container<int, input_iterator_wrapper> c2(arr2, arr2+6);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c1, c2) == 0 );
    VERIFY( lexicomp3(c2, c1) == 0 );
  }

  {
    test_container<int, input_iterator_wrapper> c1(arr1);
    test_container<int, input_iterator_wrapper> c2(arr2, arr2+7);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c1, c2) > 0 );
    VERIFY( lexicomp3(c2, c1) < 0 );
  }

  {
    test_container<int, input_iterator_wrapper> c1(arr1);
    test_container<int, forward_iterator_wrapper> c2(arr2);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c1, c2) < 0 );
    VERIFY( lexicomp3(c2, c1) > 0 );
  }

  {
    test_container<int, input_iterator_wrapper> c1(arr1);
    test_container<int, forward_iterator_wrapper> c2(arr2, arr2+7);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c2, c2) == 0 );
    VERIFY( lexicomp3(c1, c2) > 0 );
    VERIFY( lexicomp3(c2, c1) < 0 );
  }

  {
    test_container<int, forward_iterator_wrapper> c1(arr1, arr1+7);
    test_container<int, input_iterator_wrapper> c2(arr2);
    VERIFY( lexicomp3(c1, c1) == 0 );
    VERIFY( lexicomp3(c2, c2) == 0 );
    VERIFY( lexicomp3(c1, c2) < 0 );
    VERIFY( lexicomp3(c2, c1) > 0 );
  }
}

void
test02()
{
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  using __gnu_test::forward_iterator_wrapper;
  std::array<unsigned char, 8> c1 = { 0, 1, 2, 3, 4, 5, 6, 7 };
  std::array<unsigned char, 8> c2 = { 0, 1, 2, 3, 4, 5, 6, 77 };

  VERIFY( lexicomp3(c1, c1) == 0 );
  VERIFY( lexicomp3(c1, c2) < 0 );
  VERIFY( lexicomp3(c2, c1) > 0 );

  std::array<unsigned char, 7> c3 = { 0, 1, 2, 3, 4, 5, 6 };
  VERIFY( lexicomp3(c3, c3) == 0 );
  VERIFY( lexicomp3(c1, c3) > 0 );
  VERIFY( lexicomp3(c3, c1) < 0 );
}

void
test03()
{
  unsigned char a[2] = { 1, 2 };
  unsigned char* p = nullptr;
  // ensure memcmp not called with nullptr
  VERIFY( std::lexicographical_compare_three_way(p, p, a, a+2) < 0 );
  VERIFY( std::lexicographical_compare_three_way(a, a+2, p, p) > 0 );
}

int
main()
{
  test01();
  test02();
  test03();
}
