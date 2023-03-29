// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5};
  for (int i = 0; i <= 5; i++)
    for (int j = 0; j <= 5; j++)
      {
	int z[10];
	test_range<int, input_iterator_wrapper> rx(x, x+i), ry(x, x+j);
	test_range<int, output_iterator_wrapper> rz(z, z+i+j);
	auto [in1,in2,out] = ranges::merge(rx, ry, rz.begin());
	VERIFY( in1 == rx.end() );
	VERIFY( in2 == ry.end() );
	VERIFY( out == rz.end() );

	std::vector<int> v(x, x+i);
	v.insert(v.end(), x, x+j);
	ranges::sort(v);

	VERIFY( ranges::equal(v.begin(), v.end(), z, z+i+j) );
      }
}

constexpr bool
test02()
{
  int x[] = {-1,-3,-5};
  int y[] = {2,4,6};
  int z[6];
  ranges::merge(x, x+3, y, y+3, z,
		ranges::greater{}, {}, [] (int a) { return -a; });

  const int w[6] = {-1, 2, -3, 4, -5, 6};
  return ranges::equal(w, z);
}


int
main()
{
  test01();
  static_assert(test02());
}
