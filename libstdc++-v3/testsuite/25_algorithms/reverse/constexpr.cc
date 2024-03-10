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
  std::array<int, 12> ar0{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}};

  std::reverse(ar0.begin() + 2, ar0.begin() + 9);

  return ar0[2] == 8 && ar0[8] == 2;
}

static_assert(test());
