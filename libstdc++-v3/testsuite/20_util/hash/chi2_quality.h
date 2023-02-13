// Copyright (C) 2010-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// This file uses the chi^2 test to measure the quality of a hash
// function, by computing the uniformity with which it distributes a set
// of N strings into k buckets (where k is significantly greater than N).
//
// Each bucket has B[i] strings in it. The expected value of each bucket
// for a uniform distribution is z = N/k, so
//   chi^2 = Sum_i (B[i] - z)^2 / z.
//
// We check whether chi^2 is small enough to be consistent with the
// hypothesis of a uniform distribution. If F(chi^2, k-1) is close to
// 0 (where F is the cumulative probability distribution), we can
// reject that hypothesis. So we don't want F to be too small, which
// for large k, means we want chi^2 to be not too much larger than k.
//
// We use the chi^2 test for several sets of strings. Any non-horrible
// hash function should do well with purely random strings. A really
// good hash function will also do well with more structured sets,
// including ones where the strings differ by only a few bits.

#include <algorithm>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <string>
#include <unordered_set>
#include <vector>
#include <testsuite_hooks.h>

#ifndef SAMPLES
#define SAMPLES 300000
#endif

template <typename Container>
  double
  chi2_hash(const Container& c, long buckets)
  {
    std::vector<int> counts(buckets);
    std::hash<std::string> hasher;
    double elements = 0;
    for (auto i = c.begin(); i != c.end(); ++i)
      {
        ++counts[hasher(*i) % buckets];
        ++elements;
      }

    const double z = elements / buckets;
    double sum = 0;
    for (long i = 0; i < buckets; ++i)
      {
        double delta = counts[i] - z;
        sum += delta*delta;
      }
    return sum/z;
  }
