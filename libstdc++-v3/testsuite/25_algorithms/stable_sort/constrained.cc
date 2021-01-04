// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// This test doesn't verify the stability property of ranges::stable_sort,
// because at the moment it's just defined to be a wrapper over
// std::stable_sort.

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

      auto res1 = ranges::stable_sort(c.begin(), c.end(), {}, std::negate<>{});
      VERIFY( res1 == c.end() );

      auto res2 = ranges::stable_sort(r, ranges::greater{});
      VERIFY( res2 == ranges::end(r) );

      VERIFY( ranges::equal(c, r) );
      VERIFY( ranges::equal(c.begin(), c.end(), vref.rbegin(), vref.rend()) );
    }
}

int
main()
{
  test01();
}
