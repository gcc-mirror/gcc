// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-options "-std=gnu++11 -DSIMULATOR_TEST" { target simulator } }
// { dg-require-cstdint "" }

// 25.4.1 [lib.alg.sort]

#include <algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_containergen.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;

typedef test_container<int, random_access_iterator_wrapper> Container;

struct testSort
{
  template<typename Container, typename RandomGen>
  void operator()(Container con, RandomGen&)
  {
    bool test __attribute__((unused)) = true;

    std::sort(con.begin(), con.end());
    VERIFY( std::is_sorted(con.begin(), con.end()) );
  }
};

int 
main()
{
  __gnu_test::test_containers<Container>(testSort());
}
