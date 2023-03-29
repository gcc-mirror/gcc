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
// { dg-require-effective-target hosted }

#include <sstream>
#include <ranges>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  std::istringstream s("1 2 3 4 5");
  auto v = ranges::istream_view<int>(s);
  auto i = v.begin();
  static_assert(!std::semiregular<decltype(i)>);
  auto o = views::iota(std::move(i));
  static_assert(false); // { dg-error "" }
}

// { dg-prune-output "no match" }
// { dg-prune-output "deduction failed" }
// { dg-prune-output "constraint failure" }
