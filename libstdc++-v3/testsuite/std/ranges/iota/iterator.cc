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

auto i = std::ranges::iota_view<int>{}.begin();
static_assert( std::three_way_comparable<decltype(i)> );

struct Inc {
  Inc& operator++();
  Inc operator++(int);
  friend long operator-(Inc, Inc);
};
static_assert( ! std::three_way_comparable<Inc> );

// Instantiating iterator type must be valid despite !three_way_comparable<Inc>
auto j = std::ranges::iota_view<Inc>{}.begin();
static_assert( ! std::three_way_comparable<decltype(j)> );
