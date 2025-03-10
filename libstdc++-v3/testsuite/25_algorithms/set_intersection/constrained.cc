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

#include <algorithm>
#include <functional>
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
  int y[] = {3,2,1};
  int z[2];
  test_range<int, input_iterator_wrapper> rx(x), ry(y);
  test_range<int, output_iterator_wrapper> rz(z);
  auto [in1,in2,out]
    = ranges::set_intersection(rx, ry, rz.begin(), ranges::greater{});
  VERIFY( in1.ptr == x+5 );
  VERIFY( in2.ptr == y+3 );
  VERIFY( out.ptr == z+2 );
  VERIFY( ranges::equal(z, (int[]){2,1}) );
}

void
test02()
{
  int x[] = {3,2,1,1,0};
  int y[] = {3,2,1,0};
  int z[4];
  test_container<int, forward_iterator_wrapper> rx(x), ry(y);
  test_container<int, forward_iterator_wrapper> rz(z);
  auto [in1,in2,out]
    = ranges::set_intersection(rx.begin(), rx.end(),
			       ry.begin(), ry.end(),
			       rz.begin(),
			       {},
			       std::negate<>{},
			       std::negate<>{});
  VERIFY( in1.ptr == x+5 );
  VERIFY( in2.ptr == y+4 );
  VERIFY( out.ptr == z+4 );
  VERIFY( ranges::equal(z, (int[]){3,2,1,0}) );
}

constexpr bool
test03()
{
  bool ok = true;
  int x[1] = {0};
  int y[1] = {1};
  int z[1];
  ok &= ranges::set_intersection(x, x, y, y+1, z).out == z;
  ok &= ranges::set_intersection(x, x+1, y, y, z).out == z;
  return ok;
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
}
