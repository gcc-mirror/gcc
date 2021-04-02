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

#include <ranges>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

using __gnu_test::test_input_range;

namespace ranges = std::ranges;
namespace views = std::views;

struct my_range
{
  static inline int x[] = {1,2,3};
  static inline test_input_range<int> r{x};

  bool called_begin = false;

  auto
  begin()
  {
    called_begin = true;
    return r.begin();
  }

  auto
  end()
  {
    return r.end();
  }

  ranges::range_difference_t<decltype(r)>
  size()
  {
    VERIFY( !called_begin );
    return 3;
  }
};

void
test01()
{
  my_range r;
  static_assert(!ranges::forward_range<my_range>);
  static_assert(ranges::sized_range<my_range>);
  auto v = r | views::take(3);
  auto i = ranges::begin(v);
}

int
main()
{
  test01();
}
