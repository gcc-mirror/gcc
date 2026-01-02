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

#include <algorithm>
#include <ranges>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5};
  for (int i = 0; i <= 5; i++)
    for (int j = 0; j <= 5; j++)
      {
	std::vector<int> v(x, x+i);
	v.insert(v.end(), x, x+j);
	ranges::sort(v);

	test_range<int, bidirectional_iterator_wrapper> rz(v.data(), v.data()+i+j);
	auto result = ranges::inplace_merge(rz, ranges::next(ranges::begin(rz), i));
	VERIFY( result == rz.end() );

	VERIFY( ranges::is_sorted(rz) );
      }
}

void
test02()
{
  struct X { int i, j; };
  X x[] = { {1, 1}, {3, 4}, {5, 5}, {2, 2}, {2, 3} };
  auto comp = ranges::greater{};
  auto proj = [] (X a) { return -a.i; };
  ranges::inplace_merge(x, x+3, x+5, comp, proj);
  VERIFY( ranges::is_sorted(x, {}, &X::i) );
  VERIFY( ranges::is_sorted(x, {}, &X::j) );
}


void
test03()
{
  // PR libstdc++/100795 - ranges::inplace_merge should not use
  // std::inplace_merge directly
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

  ranges::sort(w | std::views::take(10));
  ranges::sort(w | std::views::drop(10));
  ranges::inplace_merge(w, w.begin() + 10);
  VERIFY( ranges::equal(w, v) );

  ranges::sort(w | std::views::take(10), std::ranges::greater{});
  ranges::sort(w | std::views::drop(10), std::ranges::greater{});
  ranges::inplace_merge(w, w.begin() + 10, std::ranges::greater{});
  VERIFY( ranges::equal(w, v | std::views::reverse) );

  ranges::sort(w | std::views::take(10), std::ranges::greater{}, std::negate{});
  ranges::sort(w | std::views::drop(10), std::ranges::greater{}, std::negate{});
  ranges::inplace_merge(w, w.begin() + 10, std::ranges::greater{}, std::negate{});
  VERIFY( ranges::equal(w, v) );
}

int
main()
{
  test01();
  test02();
  test03();
}
