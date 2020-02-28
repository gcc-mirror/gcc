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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
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

      std::ranlux48_base g1(size), g2(size + 1);
      ranges::shuffle(v1, g1);
      ranges::shuffle(v2, g2);

      for (unsigned middle = 0; middle < 10; ++middle)
	{
	  test_container<int, forward_iterator_wrapper> c
	    = {v1.data(), v1.data() + size};
	  test_range<int, input_iterator_wrapper> r
	    = {v2.data(), v2.data() + size};

	  std::vector<int> o1(middle), o2(middle);
	  test_range<int, random_access_iterator_wrapper> w1
	    = {o1.data(), o1.data()+middle};
	  test_range<int, random_access_iterator_wrapper> w2
	    = {o2.data(), o2.data()+middle};

	  auto [in1, out1] = ranges::partial_sort_copy(c.begin(), c.end(),
						       w1.begin(), w1.end(),
						       {},
						       std::negate<>{},
						       std::negate<>{});
	  VERIFY( in1 == c.end() );
	  VERIFY( out1 == w1.begin() + std::min(size, middle) );

	  auto [in2,out2] = ranges::partial_sort_copy(r, w2, ranges::greater{});
	  VERIFY( in2 == ranges::end(r) );
	  VERIFY( out2 == w2.begin() + std::min(size, middle) );

	  VERIFY( ranges::equal(w1.begin(), out1, w2.begin(), out2) );
	  VERIFY( ranges::equal(w1.begin(), out1,
				vref.rbegin(),
				vref.rbegin()+(out1-w1.begin())) );
	}
    }
}

constexpr bool
test02()
{
  int x[] = { 5,4,1,3,2 };
  int w[3];
  const int y[] = { 1,2,3 };
  ranges::partial_sort_copy(x, x+5, w, w+3);
  return ranges::equal(w, y);
}

int
main()
{
  test01();
  static_assert(test02());
}
