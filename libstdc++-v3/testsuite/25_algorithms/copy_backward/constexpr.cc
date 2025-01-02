// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include <algorithm>
#include <array>

constexpr bool
test()
{
  constexpr std::array<int, 12> ca0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};

  std::array<int, 12> ma0{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
  const auto out7 = std::copy_backward(ca0.begin(), ca0.begin() + 8,
				       ma0.begin() + 10);

  return out7 == ma0.begin() + 2 && ma0[3] == 1;
}

static_assert(test());

constexpr bool
test02()
{
  struct X
  {
    X() = default;
    X& operator=(const X&) = default;
    constexpr X& operator=(X&& x) { i = x.i; x.i = 0; return *this; }
    int i = 1;
  };

  X from[1], to[1];
  std::copy_backward(std::begin(from), std::end(from), std::end(to));
  return from[0].i == 1;
}

static_assert(test02());

constexpr bool
test03()
{
  std::array<int, 12> ma0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};
  const auto out7 = std::copy_backward(ma0.begin(), ma0.begin() + 8,
				       ma0.begin() + 10);

  return out7 == ma0.begin() + 2 && *out7 == 0 && *(ma0.begin() + 9) == 7;
}

static_assert(test03());
