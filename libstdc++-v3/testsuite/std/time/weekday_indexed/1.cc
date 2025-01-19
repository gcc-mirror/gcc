// { dg-do compile { target c++20 } }

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

// Class template day [time.cal.weekday_indexed]

#include <chrono>

constexpr void
constexpr_weekday_indexed()
{
  using namespace std::chrono;

  weekday_indexed dwdi{};

  // wdi0 is the second Sunday of an as yet unspecified month.
  constexpr auto wdi0 = Sunday[2];
  static_assert(wdi0.weekday() == Sunday);
  static_assert(wdi0.index() == 2);

  constexpr weekday_indexed wdi1 = {weekday{3}, 2};
  static_assert(wdi1.weekday() == weekday{3});
  static_assert(wdi1.index() == 2);

  static_assert(!weekday_indexed{weekday{127}, 1}.ok());
  static_assert(weekday_indexed{weekday{0}, 1}.ok());
  static_assert(weekday_indexed{weekday{6}, 2}.ok());
  static_assert(weekday_indexed{weekday{7}, 3}.ok()); // Weekday wraps 7 to 0.
  static_assert(!weekday_indexed{weekday{8}, 1}.ok());
  static_assert(!weekday_indexed{weekday{6}, 6}.ok());

  static_assert(weekday{7}[1] == weekday{0}[1]);
  static_assert(!(weekday{0}[2] == weekday{1}[2]));
  static_assert(!(weekday{0}[2] == weekday{0}[3]));
  static_assert( (weekday{0}[3] != weekday{2}[3]));
  static_assert( (weekday{0}[3] != weekday{0}[2]));
}
