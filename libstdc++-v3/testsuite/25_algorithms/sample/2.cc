// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++1z } }

#ifndef _GLIBCXX_ASSERTIONS
// Make std::uniform_int_distribution check its parameters
# define _GLIBCXX_ASSERTIONS
#endif

#include <algorithm>
#include <random>
#include <climits>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

std::mt19937 rng;

using std::sample;
using __gnu_test::test_container;
using __gnu_test::output_iterator_wrapper;

void
test01()
{
  int pop[UCHAR_MAX]  = { };
  for (int i = SCHAR_MAX; i < UCHAR_MAX; ++i)
    pop[i] = 1;
  const signed char sample_size = SCHAR_MAX; // PR libstdc++/77994
  int out[sample_size] = { };

  // random access iterators for both population and result
  // (uses reservoir sampling)
  auto it = sample(std::begin(pop), std::end(pop), out, sample_size, rng);
  auto sum = std::accumulate(out, it, 0);
  VERIFY( sum != 0 ); // exceedingly unlikely!

  // random access iterator for population and output iterator for result
  // (uses selection sampling)
  test_container<int, output_iterator_wrapper> samp2(out);
  sample(std::begin(pop), std::end(pop), samp2.begin(), sample_size, rng);
  sum = std::accumulate(std::begin(out), std::end(out), 0);
  VERIFY( sum != 0 );
}

int
main()
{
  test01();
}
