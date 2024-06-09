// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
  std::array<int, 12> ma0{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

  const auto outd = std::fill_n(ma0.begin(), 6, 77);

  return outd == ma0.begin() + 6 && ma0[5] == 77 && ma0[6] == 0;
}

static_assert(test());

constexpr bool
test_byte()
{
  // PR libstdc++/94933
  std::array<char, 12> ma0{};

  const auto outd = std::fill_n(ma0.begin(), 6, 77);

  return outd == ma0.begin() + 6 && ma0[5] == 77 && ma0[6] == 0;
}

static_assert( test_byte() );

struct S
{
  int i = 0;
};

constexpr bool
test_nonscalar()
{
  std::array<S, 12> ma0{};

  const auto outd = std::fill_n(ma0.begin(), 6, S{77});

  return outd == ma0.begin() + 6 && ma0[5].i == 77 && ma0[6].i == 0;
}

static_assert( test_nonscalar() );
