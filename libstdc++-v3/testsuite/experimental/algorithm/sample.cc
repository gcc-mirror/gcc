// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }
// { dg-require-cstdint "" }

#include <experimental/algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

std::mt19937 rng;

void
test01()
{
  const int pop[] = { 1, 2 };
  int samp[10] = { };

  // population smaller than desired sample size
  auto it = std::experimental::sample(pop, pop + 2, samp, 10, rng);
  VERIFY( it == samp + 2 );
  VERIFY( std::accumulate(samp, samp + 10, 0) == 3 );
}

void
test02()
{
  const int pop[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
  int samp[10] = { };

  auto it = std::experimental::sample(pop, std::end(pop), samp, 10, rng);
  VERIFY( it == samp + 10 );

  std::sort(samp, it);
  auto it2 = std::unique(samp, it);
  VERIFY( it2 == it );
}

void
test03()
{
  const int pop[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, };
  int samp[5] = { };

  // input iterator for population
  test_container<const int, input_iterator_wrapper> pop_in{pop};
  auto it = std::experimental::sample(pop_in.begin(), pop_in.end(),
                                      samp,
                                      5, rng);
  VERIFY( it == samp + 5 );

  std::sort(samp, it);
  auto it2 = std::unique(samp, it);
  VERIFY( it2 == it );
}

void
test04()
{
  const int pop[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  int samp[5] = { };

  // forward iterator for population and output iterator for result
  test_container<const int, forward_iterator_wrapper> pop_fwd{pop};
  test_container<int, output_iterator_wrapper> samp_out{samp};
  auto it = std::experimental::sample(pop_fwd.begin(), pop_fwd.end(),
				      samp_out.begin(), 5, rng);

  VERIFY( std::distance(samp, it.ptr) == 5 );

  std::sort(samp, it.ptr);
  auto it2 = std::unique(samp, it.ptr);
  VERIFY( it2 == it.ptr );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
