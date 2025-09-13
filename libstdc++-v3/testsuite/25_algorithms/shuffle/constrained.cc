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
#include <ranges>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

// This test is adapted from 25_algorithms/shuffle/1.cc.

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
      VERIFY( ranges::shuffle(c, g1) == c.end() );
      VERIFY( ranges::shuffle(ranges::begin(r), ranges::end(r), g2)
	      == ranges::end(r) );

      if (size >= 10)
	{
	  VERIFY( !ranges::equal(c, vref) );
	  VERIFY( !ranges::equal(r, vref) );
	  VERIFY( !ranges::equal(c, r) );
	}

      VERIFY( ranges::is_permutation(c, vref) );
      VERIFY( ranges::is_permutation(r, vref) );
    }
}

void
test02()
{
  // PR libstdc++/100795 - ranges::shuffle should not use std::shuffle directly
#if 0 // FIXME: ranges::shuffle rejects integer-class difference types.
#if __SIZEOF_INT128__
  auto v = std::views::iota(__int128(0), __int128(20));
#else
  auto v = std::views::iota(0ll, 20ll);
#endif
#else
  auto v = std::views::iota(0, 20);
#endif

  int storage[20] = {2,5,4,3,1,6,7,9,10,8,11,14,12,13,15,16,18,0,19,17};
  auto w = v | std::views::transform([&](auto i) -> int& { return storage[i]; });
  using type = decltype(w);
  static_assert( std::ranges::random_access_range<type> );

  std::ranlux48_base g;
  ranges::shuffle(w, g);
}

struct non_default_sentinel_t { };

template<std::input_or_output_iterator I>
bool operator==(const I& i, non_default_sentinel_t)
{ return i == std::default_sentinel; }

void
test03()
{
  // PR libstdc++/121917 - ranges::shuffle incorrectly requires its arguments
  // to model sized_sentinel_for
  int a[2]{};
  std::counted_iterator iter(a, 2);
  std::default_random_engine e;
  std::ranges::shuffle(iter, non_default_sentinel_t{}, e);
}

int
main()
{
  test01();
  test02();
  test03();
}
