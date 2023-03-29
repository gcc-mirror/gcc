// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
// { dg-require-debug-mode { } }

#include <algorithm>
#include <array>

struct A
{
  int _i;

  constexpr bool
  operator<(const A& a) const
  { return _i < a._i; }
};

constexpr bool
test()
{
  constexpr std::array<A, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 5, 8, 9, 10, 11}};

  constexpr A a6{ 6 };
  const auto it = std::lower_bound(ca0.begin(), ca0.end(), a6);

  return true;
}

static_assert(test()); // { dg-error "" }

// { dg-prune-output "builtin_unreachable" }
// { dg-prune-output "in 'constexpr'" }
