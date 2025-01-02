// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// Verify P2367 changes.

#include <ranges>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  extern int (&x)[5];

  // Verify changes to views::single.
  using ranges::single_view;
  using std::same_as;
  same_as<single_view<int*>> auto v1 = views::single(x);
  same_as<single_view<int>> auto v2 = views::single((const int)5);
  same_as<single_view<single_view<int>>> auto v3 = views::single(v2);

  // Verify changes to views::take.
  auto v4 = views::take(x, 0ull);

  // Verify changes to views::drop.
  auto v5 = views::drop(x, 0ull);

  // Verify changes to views::lazy_split.
  auto v6 = views::lazy_split(x, 5u);
  auto v7 = views::split(x, 5u);
}
