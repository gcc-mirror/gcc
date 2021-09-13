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

// Class template day [time.cal.weekday_last]

#include <chrono>

constexpr void
constexpr_weekday_last()
{
  using namespace std::chrono;

  constexpr auto wdl0 = Sunday[last];
  static_assert(wdl0.weekday() == Sunday);

  constexpr auto wdl1 = weekday{3}[2];
  static_assert(wdl1.weekday() == weekday{3});
  static_assert(wdl1.index() == 2);
  constexpr auto wdll = weekday{3}[last];
  static_assert(wdll.weekday() == weekday{3});

  static_assert(!weekday_last{weekday{127}}.ok());
  static_assert(weekday_last{weekday{0}}.ok());
  static_assert(weekday_last{weekday{6}}.ok());
  static_assert(weekday_last{weekday{7}}.ok()); // Weekday wraps 7 to 0.
  static_assert(!weekday_last{weekday{8}}.ok());

  static_assert( (weekday_last{weekday{7}} == weekday_last{weekday{0}}));
  static_assert(!(weekday_last{weekday{0}} == weekday_last{weekday{1}}));
  static_assert( (weekday_last{weekday{0}} != weekday_last{weekday{2}}));
}
