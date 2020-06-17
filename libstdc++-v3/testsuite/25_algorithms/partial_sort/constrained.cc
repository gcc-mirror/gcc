// Copyright (C) 2020 Free Software Foundation, Inc.
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
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  for (unsigned size = 0; size < 50; ++size)
    {
      std::vector<int> vref(size);
      std::iota(vref.begin(), vref.end(), 0);
      std::vector<int> v1(vref), v2(vref);
      test_container<int, random_access_iterator_wrapper> c
	= {v1.data(), v1.data() + size};
      test_range<int, random_access_iterator_wrapper> r
	= {v2.data(), v2.data() + size};

      std::ranlux48_base g1(size), g2(size + 1);
      ranges::shuffle(c, g1);
      ranges::shuffle(ranges::begin(r), ranges::end(r), g2);

      for (unsigned middle = 0; middle < std::min(size, 10U); ++middle)
	{
	  auto res1 = ranges::partial_sort(c.begin(), c.begin()+middle, c.end(),
					   {}, std::negate<>{});
	  VERIFY( res1 == c.end() );

	  auto res2 = ranges::partial_sort(r,
					   ranges::begin(r)+middle,
					   ranges::greater{});
	  VERIFY( res2 == ranges::end(r) );

	  VERIFY( ranges::equal(c.begin(), c.begin()+middle,
				r.begin(), r.begin()+middle) );
	  VERIFY( ranges::equal(c.begin(), c.begin()+middle,
				vref.rbegin(), vref.rbegin()+middle) );
	}
    }
}

constexpr bool
test02()
{
  int x[] = { 5,4,1,3,2 };
  const int y[] = { 1,2,3 };
  ranges::partial_sort(x, x+3, x+5);
  return ranges::equal(x, x+3, y, y+3);
}

int
main()
{
  test01();
  static_assert(test02());
}
