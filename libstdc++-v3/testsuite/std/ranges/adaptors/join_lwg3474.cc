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
// { dg-do compile { target c++2a } }

#include <ranges>
#include <vector>

void
test01()
{
  // LWG 3474. Nesting join_views is broken because of CTAD
  std::vector<std::vector<std::vector<int>>> nested_vectors = {
    {{1, 2, 3}, {4, 5}, {6}},
    {{7},       {8, 9}, {10, 11, 12}},
    {{13}}
  };
  auto joined = nested_vectors | std::views::join | std::views::join;

  using V = decltype(joined);
  static_assert( std::same_as<std::ranges::range_value_t<V>, int> );
}
