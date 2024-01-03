// { dg-do compile { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// Class template year_month_day [time.cal.year_month_day]

#include <chrono>

constexpr void
constexpr_year_month_day_op_overload_disambiguation()
{
  using namespace std::chrono;
  using decades = duration<long long, std::ratio<31556952 * 10>>;
  static_assert(std::convertible_to<decades, months>
		&& std::convertible_to<decades, years>);
  using ymd = year_month_day;

  constexpr ymd ymd1 = 2015y/June/15d;
  static_assert(ymd1 + decades{1} == 2025y/June/15d);
  static_assert(ymd1 - decades{1} == 2005y/June/15d);
  static_assert(decades{1} + ymd1 == 2025y/June/15d);
  static_assert((ymd{ymd1} += decades{1}) == 2025y/June/15d);
  static_assert((ymd{ymd1} -= decades{1}) == 2005y/June/15d);
}
