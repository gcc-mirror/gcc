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

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i, j;
};

void
test01()
{
  VERIFY( ranges::clamp(1, 2, 4) == 2 );
  VERIFY( ranges::clamp(3, 2, 4) == 3 );
  VERIFY( ranges::clamp(5, 2, 4) == 4 );

  VERIFY( ranges::clamp(1, 4, 2, ranges::greater{}) == 2 );
  VERIFY( ranges::clamp(3, 4, 2, ranges::greater{}) == 3 );
  VERIFY( ranges::clamp(5, 4, 2, ranges::greater{}) == 4 );

  VERIFY( ranges::clamp(1, 2, 4, ranges::greater{}, std::negate<>{}) == 2 );
  VERIFY( ranges::clamp(3, 2, 4, ranges::greater{}, std::negate<>{}) == 3 );
  VERIFY( ranges::clamp(5, 2, 4, ranges::greater{}, std::negate<>{}) == 4 );

  static_assert(ranges::clamp(X{1,2}, X{1,3}, X{1,4}, {}, &X::i).j == 2);
}

int
main()
{
  test01();
}
