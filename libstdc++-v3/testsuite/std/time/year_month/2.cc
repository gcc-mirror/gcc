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

// Class template year_month [time.cal.year_month]

#include <chrono>

constexpr void
constexpr_year_month_op_overload_disambiguation()
{
  using namespace std::chrono;
  using decades = duration<long long, std::ratio<31556952 * 10>>;
  static_assert(std::convertible_to<decades, months>
		&& std::convertible_to<decades, years>);
  using ym = year_month;

  constexpr ym ym1 = 2015y/June;
  static_assert(ym1 + decades{1} == 2025y/June);
  static_assert(ym1 - decades{1} == 2005y/June);
  static_assert(decades{1} + ym1 == 2025y/June);
  static_assert((ym{ym1} += decades{1}) == 2025y/June);
  static_assert((ym{ym1} -= decades{1}) == 2005y/June);
}
