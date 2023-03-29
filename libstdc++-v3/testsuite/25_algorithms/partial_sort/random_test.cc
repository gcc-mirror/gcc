// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

// 25.4.1.3 [lib.alg.partial.sort]

#include <algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_containergen.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, random_access_iterator_wrapper> Container;

struct testPartialSort
{
  template<typename Container, typename RandomGen>
  void operator()(Container con, RandomGen& rg)
  {
    const int size = con.end() - con.begin();
    auto dist = std::uniform_int_distribution<>(0, size);
    const int element = dist(rg);

    std::partial_sort(con.begin(), con.begin() + element, con.end());

    VERIFY( std::is_sorted(con.begin(), con.begin() + element) );

    if (element > 0)
      {
        for (int i = element; i < size; ++i)
	  VERIFY( con.val(element - 1) <= con.val(i) );
      }
  }
};

int 
main()
{
  __gnu_test::test_containers<Container>(testPartialSort());
}
