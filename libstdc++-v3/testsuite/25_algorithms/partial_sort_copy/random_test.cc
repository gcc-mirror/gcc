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

// 25.4.1.4 [lib.alg.partial.sort.copy]

#include <algorithm>
#include <random>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_containergen.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, random_access_iterator_wrapper> Container;

struct testPartialSortCopy
{
  template<typename Container, typename RandomGen>
  void operator()(Container con, RandomGen& rg)
  {
    const int size = con.end() - con.begin();
    auto dist = std::uniform_int_distribution<>(0, size);
    const int element = dist(rg);

    std::vector<int> outvec(element + 1); // add +1 to avoid empty issues

    Container out(outvec.data(), outvec.data() + element);

    std::partial_sort_copy(con.begin(), con.end(),
			   out.begin(), out.begin() + element);

    VERIFY( std::is_sorted(out.begin(), out.begin() + element) );

    std::sort(con.begin(), con.end());

    for (int i = 0; i < element; ++i)
      VERIFY( con.val(i) == out.val(i) );
  }
};

int 
main()
{
  __gnu_test::test_containers<Container>(testPartialSortCopy());
}
