// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

std::mt19937 rng;

using std::sample;
using __gnu_test::test_container;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

void
test01()
{
  const int in = 0;
  test_container<const int, forward_iterator_wrapper> pop(&in, &in);
  int out;
  test_container<int, output_iterator_wrapper> samp(&out, &out + 1);

  auto it = sample(pop.begin(), pop.end(), samp.begin(), 1, rng);
  VERIFY( it.ptr == &out );
}

int
main()
{
  test01();
}
