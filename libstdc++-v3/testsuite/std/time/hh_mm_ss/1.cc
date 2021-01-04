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

// Class template hh_mm_ss [time.hh_mm_ss]

#include <chrono>

constexpr void
constexpr_hh_mm_ss()
{
  using namespace std::chrono;
  using std::ratio;

  static_assert(hh_mm_ss<hours>::fractional_width == 0);
  static_assert(hh_mm_ss<minutes>::fractional_width == 0);
  static_assert(hh_mm_ss<seconds>::fractional_width == 0);
  static_assert(hh_mm_ss<milliseconds>::fractional_width == 3);
  static_assert(hh_mm_ss<microseconds>::fractional_width == 6);
  static_assert(hh_mm_ss<nanoseconds>::fractional_width == 9);
  static_assert(hh_mm_ss<duration<int, ratio<1, 2>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 3>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 4>>>::fractional_width == 2);
  static_assert(hh_mm_ss<duration<int, ratio<2, 4>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 5>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 6>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 7>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 8>>>::fractional_width == 3);
  static_assert(hh_mm_ss<duration<int, ratio<1, 9>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 10>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<756, 625>>>::fractional_width == 4);
  static_assert(hh_mm_ss<duration<int, ratio<1, (1ll << 62)>>>::fractional_width
		== 18);

  constexpr auto hms = hh_mm_ss{-(8h + 9min + 10s + 11ms + 12us + 13ns)};
  static_assert(__is_same(decltype(hms)::precision, nanoseconds));
  static_assert(hms.hours() == 8h);
  static_assert(hms.minutes() == 9min);
  static_assert(hms.seconds() == 10s);
  static_assert(hms.subseconds() == 11ms + 12us + 13ns);
  static_assert(hms.is_negative());
  static_assert(hh_mm_ss{hms.to_duration()}.to_duration() == hms.to_duration());

  static_assert(seconds{hh_mm_ss{100min}} == 100min);

  // TODO: treat_as_floating_point_v
}
