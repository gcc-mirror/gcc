// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[50];
  std::iota(x, x+50, 0);

  auto pred = std::greater{};
  auto proj = [] (int a) { return -a; };
  for (std::ptrdiff_t i = 0; i < 50; i++)
    {
      test_range<int, random_access_iterator_wrapper> rx(x);
      std::ranlux48_base g(i);
      ranges::shuffle(rx, g);

      auto result = ranges::nth_element(rx, rx.begin()+i, pred, proj);
      VERIFY( result == rx.end() );
      VERIFY( x[i] == i );
      for (int j = 0; j < i; j++)
	for (int k = i; k < 50; k++)
	  VERIFY( !pred(proj(x[k]), proj(x[j])) );

      result = ranges::nth_element(rx, rx.begin()+i, pred);
      VERIFY( result == rx.end() );
      VERIFY( x[i] == 49-i );
      for (int j = 0; j < i; j++)
	for (int k = i; k < 50; k++)
	  VERIFY( !pred(x[k], x[j]) );
    }
}

constexpr bool
test02()
{
  int x[] = {5,2,1,3,4};
  ranges::nth_element(x, x+3);
  return x[3] == 4;
}

constexpr bool
test03()
{
  // PR libstdc++/100795 - ranges::sort should not use std::sort directly
#if __SIZEOF_INT128__
  auto v = std::views::iota(__int128(0), __int128(20));
#else
  auto v = std::views::iota(0ll, 20ll);
#endif

  int storage[20] = {2,5,4,3,1,6,7,9,10,8,11,14,12,13,15,16,18,0,19,17};
  auto w = v | std::views::transform([&](auto i) -> int& { return storage[i]; });
  using type = decltype(w);
  using cat = std::iterator_traits<std::ranges::iterator_t<type>>::iterator_category;
  static_assert( std::same_as<cat, std::output_iterator_tag> );
  static_assert( std::ranges::random_access_range<type> );

  ranges::nth_element(w, w.begin() + 10);
  VERIFY( w[10] == 10 );

  ranges::nth_element(w, w.begin() + 5, std::ranges::greater{});
  VERIFY( w[5] == 19 - 5 );

  ranges::nth_element(w, w.begin() + 15, std::ranges::greater{}, std::negate{});
  VERIFY( w[15] == 15 );

  return true;
}

int
main()
{
  test01();
  static_assert(test02());
  static_assert(test03());
}
