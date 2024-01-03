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

// { dg-do run { target c++11 } }
// { dg-require-normal-mode "" }
// { dg-require-cmath "" }
// { dg-require-cstdint "" }
// { dg-require-effective-target random_device }

#include <cmath>
#include <random>
#include <vector>
#include <algorithm>

#include <testsuite_counter_type.h>
#include <testsuite_hooks.h>

void test01()
{
  using __gnu_test::counter_type;
  const std::size_t nb_values = 1000;

  std::random_device dev;
  std::uniform_int_distribution<int> dist;
  std::vector<counter_type> values;
  values.reserve(nb_values);
  for (std::size_t i = 0; i != nb_values; ++i)
    values.push_back(dist(dev));

  std::make_heap(values.begin(), values.end());

  counter_type::reset();

  std::pop_heap(values.begin(), values.end());

  VERIFY( counter_type::less_compare_count <= 2.0 * std::log2(nb_values) );
}

int main()
{
  test01();
  return 0;
}
