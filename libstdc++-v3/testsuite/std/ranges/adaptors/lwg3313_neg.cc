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
// { dg-do compile { target c++2a } }

#include <array>
#include <iterator>
#include <ranges>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  int x[] = {1};
  auto rx = ranges::subrange{std::counted_iterator(x,1), std::default_sentinel};
  std::array<decltype(rx), 5> y = {rx, rx, rx, rx, rx,};
  auto v = y | views::join;
  auto i = v.begin();
  ++i;
  ++i;
  --i; // { dg-error "no match" }
  i--; // { dg-error "" }
}
