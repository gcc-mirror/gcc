// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// PR libstdc++/97512
// Check that structured bindings work for subranges without <ranges>.
#include <algorithm>

constexpr bool
test01()
{
  int r[] = { 1, 2, 2, 3, 3, 3 };
  auto [first, last] = std::ranges::unique(r);
  return first == std::ranges::begin(r) + 3 && last == std::ranges::end(r);
}

static_assert( test01() );
