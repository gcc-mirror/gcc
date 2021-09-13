// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

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

// Function is_pm [time.12]

#include <chrono>

constexpr void
constexpr_make24()
{
  using namespace std::chrono;

  static_assert(make24(0h, false) == 0h);
  static_assert(make24(1h, false) == 1h);
  static_assert(make24(5h, false) == 5h);
  static_assert(make24(11h, false) == 11h);
  static_assert(make24(12h, false) == 0h);

  static_assert(make24(0h, true) == 12h);
  static_assert(make24(1h, true) == 13h);
  static_assert(make24(5h, true) == 17h);
  static_assert(make24(11h, true) == 23h);
  static_assert(make24(12h, true) == 12h);
}
