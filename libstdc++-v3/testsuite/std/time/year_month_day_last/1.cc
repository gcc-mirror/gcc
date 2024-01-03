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

// Class template day [time.cal.year_month_day_last]

#include <chrono>

constexpr void
constexpr_year_month_day_last()
{
  using namespace std::chrono;
  using mdl = month_day_last;
  using ymdl = year_month_day_last;

  year_month_day_last ymdl1{year{1066}, mdl{October}};
  ymdl1 += months{9};
  ymdl1 -= months{9};
  ymdl1 += years{12};
  ymdl1 -= years{12};

  constexpr ymdl ymdl2{year{1984}, mdl{August}};
  static_assert(ymdl2.year() == year{1984});
  static_assert(ymdl2.month() == August);
  static_assert(ymdl2.month_day_last() == mdl{August});
  static_assert(ymdl2.day() == day{31});
  static_assert(sys_days(ymdl2).time_since_epoch().count() == 5356);
  static_assert(local_days(ymdl2).time_since_epoch().count() == 5356);

  static_assert( (ymdl{year{1984}, mdl{August}}.ok()));
  static_assert(!(ymdl{year{1984}, mdl{month{13}}}.ok()));

  static_assert(2015y/August/last == ymdl{year{2015}, month_day_last{month{8}}});
  static_assert(2015y/(August/last) == ymdl{year{2015}, month_day_last{month{8}}});
  static_assert(2015/(August/last) == ymdl{year{2015}, month_day_last{month{8}}});
  static_assert(August/last/2015y == ymdl{year{2015}, month_day_last{month{8}}});
  static_assert(August/last/2015 == ymdl{year{2015}, month_day_last{month{8}}});

  static_assert(January/last/2000 <=> January/last/2000
		== std::strong_ordering::equal);
  static_assert(January/last/2000 <=> February/last/2000
		== std::strong_ordering::less);
  static_assert(January/last/2000 <=> January/last/1999
		== std::strong_ordering::greater);

  static_assert(January/last/2000 + months{13} == February/last/2001);
  static_assert(January/last/2000 + months{-1} == December/last/1999);
  static_assert(January/last/2000 - months{13} == December/last/1998);
  static_assert(January/last/2000 - months{-13} == February/last/2001);

  static_assert(January/last/2000 + years{5} == January/last/2005);
  static_assert(January/last/2000 - years{5} == January/last/1995);

  static_assert(year_month_day{January/last/2000} == January/31/2000);
  static_assert(year_month_day{February/last/2000} == February/29/2000);
  static_assert(year_month_day{March/last/2000} == March/31/2000);
  static_assert(year_month_day{April/last/2000} == April/30/2000);
  static_assert(year_month_day{May/last/2000} == May/31/2000);
  static_assert(year_month_day{June/last/2000} == June/30/2000);
  static_assert(year_month_day{July/last/2000} == July/31/2000);
  static_assert(year_month_day{August/last/2000} == August/31/2000);
  static_assert(year_month_day{September/last/2000} == September/30/2000);
  static_assert(year_month_day{October/last/2000} == October/31/2000);
  static_assert(year_month_day{November/last/2000} == November/30/2000);
  static_assert(year_month_day{December/last/2000} == December/31/2000);
}
