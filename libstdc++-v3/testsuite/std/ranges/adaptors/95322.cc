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

// { dg-options "-std=gnu++20" }
// { dg-do run { target c++2a } }

#include <ranges>
#include <testsuite_iterators.h>

using __gnu_test::test_forward_range;

void
test01()
{
  // PR libstdc++/95322
  int a[2]{1, 2};
  test_forward_range<int> v{a};
  auto view1 = v | std::views::take(2);
  auto view2 = view1 | std::views::transform(std::identity{});
  const bool eq = std::ranges::cbegin(view2) == std::ranges::end(view2);
  VERIFY( !eq );
}

void
test02()
{
  using irange = test_forward_range<int>;

  int a[2]{1, 2};
  int b[3]{3, 4, 5};
  irange u[2]{ irange{a}, irange{b} };
  test_forward_range<irange> v{u};
  auto view = (std::views::counted(v.begin(), 2)
	       | std::views::transform(std::identity{})
	       | std::views::join);
  const bool eq = std::ranges::cbegin(view) == std::ranges::end(view);
  VERIFY( !eq );
}

int main()
{
  test01();
  test02();
}
