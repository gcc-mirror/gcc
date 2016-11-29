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

// { dg-do run { target c++14 } }

#include <experimental/algorithm>
#include <iterator>
#include <sstream>
#include <forward_list>
#include <vector>
#include <random>
#include <algorithm>
#include <testsuite_hooks.h>

std::mt19937 rng;

using std::istream_iterator;
using std::ostream_iterator;

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
  std::istringstream pop("0 1 2 3 4 5 6 7 8 9");
  int samp[5] = { };

  // input iterator for population
  auto it = std::experimental::sample(istream_iterator<int>{pop}, {},
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
  std::forward_list<int> pop{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  std::stringstream samp;

  // forward iterator for population and output iterator for result
  std::experimental::sample(pop.begin(), pop.end(),
                            ostream_iterator<int>{samp, " "},
                            5, rng);

  // samp.rdbuf()->pubseekoff(0, std::ios::beg);
  std::vector<int> v(istream_iterator<int>{samp}, {});
  VERIFY( v.size() == 5 );

  std::sort(v.begin(), v.end());
  auto it = std::unique(v.begin(), v.end());
  VERIFY( it == v.end() );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
