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
// { dg-additional-options "-O -Wall" }
// { dg-do compile { target c++2a } }

#include <ranges>
#include <vector>

namespace ranges = std::ranges;
namespace views = std::views;

auto
test()
{
  std::vector<std::string> x = {""};
  auto i = std::counted_iterator(x.begin(), 1);
  auto r = ranges::subrange{i, std::default_sentinel};
  auto v = r | views::join;
  return v;
}
