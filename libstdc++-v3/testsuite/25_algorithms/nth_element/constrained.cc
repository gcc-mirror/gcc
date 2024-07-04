// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
  for (int i = 0; i < 50; i++)
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

int
main()
{
  test01();
  static_assert(test02());
}
