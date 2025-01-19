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
// { dg-require-debug-mode "" }

#include <algorithm>
#include <array>

constexpr bool
test01()
{
  constexpr std::array<int, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};
  constexpr std::array<int, 12> ca1{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};

  const auto outa = std::equal(ca0.end(), ca0.begin(), ca1.begin());
  return outa;
}

static_assert(test01()); // { dg-error "non-constant condition" }
// { dg-error "_Error_formatter::_M_error()" "" { target *-*-* } 0 }

constexpr bool
test02()
{
  constexpr std::array<int, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};
  constexpr std::array<int, 11> ca1{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}};

  const auto outa = std::equal(ca0.begin(), ca0.end(), ca1.begin());
  return outa;
}

static_assert(test02()); // { dg-error "non-constant condition" }
// { dg-error "is outside the bounds" "" { target *-*-* } 0 }
