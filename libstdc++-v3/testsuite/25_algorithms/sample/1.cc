// Copyright (C) 2014-2016 Free Software Foundation, Inc.
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

#include <algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

std::mt19937 rng;

using std::sample;
using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

void
test01()
{
  const int in[] = { 1, 2 };
  test_container<const int, random_access_iterator_wrapper> pop(in);
  const int sample_size = 10;
  int samp[sample_size] = { };

  // population smaller than desired sample size
  auto it = sample(pop.begin(), pop.end(), samp, sample_size, rng);
  VERIFY( it == samp + std::distance(pop.begin(), pop.end()) );
  const auto sum = std::accumulate(pop.begin(), pop.end(), 0);
  VERIFY( std::accumulate(samp, samp + sample_size, 0) == sum );
}

void
test02()
{
  const int in[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
  test_container<const int, random_access_iterator_wrapper> pop(in);
  const int sample_size = 10;
  int samp[sample_size] = { };

  auto it = sample(pop.begin(), pop.end(), samp, sample_size, rng);
  VERIFY( it == samp + sample_size );

  // verify no duplicates
  std::sort(samp, it);
  auto it2 = std::unique(samp, it);
  VERIFY( it2 == it );
}

void
test03()
{
  const int in[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  test_container<const int, input_iterator_wrapper> pop(in);
  const int sample_size = 5;
  int samp[sample_size] = { };

  // input iterator for population
  auto it = sample(pop.begin(), pop.end(), samp, sample_size, rng);
  VERIFY( it == samp + sample_size );

  // verify no duplicates
  std::sort(samp, it);
  auto it2 = std::unique(samp, it);
  VERIFY( it2 == it );
}

void
test04()
{
  const int in[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  test_container<const int, forward_iterator_wrapper> pop(in);
  const int sample_size = 5;
  int out[sample_size];
  test_container<int, output_iterator_wrapper> samp(out);

  // forward iterator for population and output iterator for result
  auto res = sample(pop.begin(), pop.end(), samp.begin(), sample_size, rng);

  // verify no duplicates
  std::sort(std::begin(out), std::end(out));
  auto it = std::unique(std::begin(out), std::end(out));
  VERIFY( it == std::end(out) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
