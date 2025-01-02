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
#include <functional>

constexpr bool
test()
{
  auto ok = true;

  std::array<int, 23>
  ah{{0,
      1, 2,
      3, 4, 5, 6,
      7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22}};

  std::make_heap(ah.begin(), ah.begin() + 17);
  ok = ok && std::is_heap(ah.begin(), ah.begin() + 17);

  std::sort_heap(ah.begin(), ah.begin() + 17);
  ok = ok && std::is_sorted(ah.begin(), ah.begin() + 17);

  std::make_heap(ah.begin(), ah.begin() + 17, std::greater<>());
  ok = ok && std::is_heap(ah.begin(), ah.begin() + 17, std::greater<>());

  std::sort_heap(ah.begin(), ah.begin() + 17, std::greater<>());
  ok = ok && std::is_sorted(ah.begin(), ah.begin() + 17, std::greater<>());

  return ok;
}

static_assert(test());
