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

// { dg-options "-DSIMULATOR_TEST" { target simulator } }
// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 25.4.2 [lib.alg.nth.element]

#include <algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_containergen.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, random_access_iterator_wrapper> Container;

struct testNthElement
{
  template<typename Container, typename RandomGen>
  void operator()(Container con, RandomGen& rg)
  {
    const int size = con.end() - con.begin();
    auto dist = std::uniform_int_distribution<>(0, size);
    const int element = dist(rg);

    std::nth_element(con.begin(), con.begin() + element, con.end());

    if (element < size)
      {
        for (int i = 0; i < element; ++i)
	  VERIFY( con.val(i) <= con.val(element) );

	for (int i = element + 1; i < size; ++i)
	  VERIFY( con.val(i) >= con.val(element) );
      }
  }
};

int 
main()
{
  __gnu_test::test_containers<Container>(testNthElement());
}
