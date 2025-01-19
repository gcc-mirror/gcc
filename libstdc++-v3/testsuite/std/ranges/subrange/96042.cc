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

// { dg-do compile { target c++20 } }
// { dg-add-options strict_std }

#include <ranges>
#include <limits>

constexpr bool
test01()
{
  using I = unsigned long long;
  // view with a difference type that doesn't fit in long long:
  std::ranges::iota_view<I, I> v(0, std::numeric_limits<I>::max());
  // view with a size type that doesn't fit in unsigned long long:
  std::ranges::subrange sr{v.begin(), v.end()};
  auto sz = std::ranges::size(sr);
  return sz == std::numeric_limits<I>::max();
}
static_assert( test01() );
