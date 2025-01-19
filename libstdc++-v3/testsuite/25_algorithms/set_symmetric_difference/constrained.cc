// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {4,2,1,1,0};
  int y[] = {3,2,2,1};
  int z[5];
  test_range<int, input_iterator_wrapper> rx(x), ry(y);
  test_range<int, output_iterator_wrapper> rz(z);
  auto [in1,in2,out]
    = ranges::set_symmetric_difference(rx, ry, rz.begin(), ranges::greater{});
  VERIFY( in1.ptr == x+5 );
  VERIFY( in2.ptr == y+4 );
  VERIFY( out.ptr == z+5 );
  VERIFY( ranges::equal(z, (int[]){4,3,2,1,0}) );
}

void
test02()
{
  int x[] = {3,2,1,1,0};
  int y[] = {3,2,1,0};
  int z[1];
  test_container<int, forward_iterator_wrapper> rx(x), ry(y);
  test_container<int, forward_iterator_wrapper> rz(z);
  auto [in1,in2,out]
    = ranges::set_symmetric_difference(rx.begin(), rx.end(),
				       ry.begin(), ry.end(),
				       rz.begin(),
				       {},
				       std::negate<>{},
				       std::negate<>{});
  VERIFY( in1.ptr == x+5 );
  VERIFY( in2.ptr == y+4 );
  VERIFY( out.ptr == z+1 );
  VERIFY( ranges::equal(z, (int[]){1}) );
}

constexpr bool
test03()
{
  bool ok = true;
  int x[1] = {0};
  int y[1] = {1};
  int z[1];
  ok &= ranges::set_symmetric_difference(x, x, y, y+1, z).out == z+1;
  ok &= z[0] == 1;
  ok &= ranges::set_symmetric_difference(x, x+1, y, y, z).out == z+1;
  ok &= z[0] == 0;
  return ok;
}

void
test04()
{
  int x[15] = {5,5,5,5,5,4,4,4,4,3,3,3,2,2,1};
  int y[15] = {5,5,5,5,5,4,4,4,4,3,3,3,2,2,1};
  for (int k = 0; k < 100; k++)
    {
      std::ranlux48_base g(k);
      ranges::shuffle(x, g);
      ranges::shuffle(y, g);
      ranges::sort(x, x+10);
      ranges::sort(y, y+10);

      int z[15];
      auto z_out = ranges::set_symmetric_difference(x, x+10, y, y+10, z).out;

      int w1[15];
      auto w1_out = ranges::set_difference(x, x+10, y, y+10, w1).out;

      int w2[15];
      auto w2_out = ranges::set_difference(y, y+10, x, x+10, w2).out;

      int w3[15];
      auto w3_out = ranges::set_union(w1, w1_out, w2, w2_out, w3).out;

      VERIFY( ranges::equal(z, z_out, w3, w3_out) );
    }
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
  test04();
}
