// Copyright (C) 2020-2022 Free Software Foundation, Inc.
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

#include <algorithm>
#include <ranges>
#include <string_view>

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  using namespace std::literals;
  auto x = "the  quick  brown  fox"sv;
  auto v = views::lazy_split(x, std::initializer_list<char>{' ', ' '}); // { dg-error "no match" }
}

void
test02()
{
  using namespace std::literals;
  auto x = "the  quick  brown  fox"sv;
  auto v1 = views::lazy_split(std::initializer_list<char>{' ', ' '})(x); // { dg-error "deleted" }
  auto v2 = x | views::lazy_split(std::initializer_list<char>{' ', ' '}); // { dg-error "no match" }
}

// { dg-prune-output "in requirements" }
