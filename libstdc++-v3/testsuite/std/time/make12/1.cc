// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

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

// Function is_pm [time.12]

#include <chrono>

constexpr void
constexpr_make12()
{
  using namespace std::chrono;
  static_assert(make12(0h) == 12h);
  static_assert(make12(1h) == 1h);
  static_assert(make12(5h) == 5h);
  static_assert(make12(12h) == 12h);
  static_assert(make12(13h) == 1h);
  static_assert(make12(19h) == 7h);
  static_assert(make12(23h) == 11h);
}
