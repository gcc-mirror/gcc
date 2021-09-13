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

// Class template day [time.cal.year_month_day]

#include <chrono>

constexpr void
constexpr_year_month_day()
{
  using namespace std::chrono;
  using ymd = year_month_day;

  constexpr ymd ymd1{year{1984}, August, 3d};
  static_assert(ymd1.ok());
  static_assert(ymd1.year() == year{1984});
  static_assert(ymd1.month() == August);
  static_assert(ymd1.day() == 3d);
  //static_assert(sys_days(ymd1) == time_point_cast<days>(days{5356}));
  //static_assert(local_days(ymd1) == time_point_cast<days>(days{5356}));

  static_assert(2015y/August/14d == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015y/August/14 == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015y/(August/14d) == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015/(August/14d) == ymd{year{2015}, month{8}, day{14}});
  static_assert(August/14d/2015y == ymd{year{2015}, month{8}, day{14}});
  static_assert(August/14d/2015 == ymd{year{2015}, month{8}, day{14}});

  static_assert(((ymd{1000y, January, 1d} += months{1}) += years{1})
		== February/1d/1001y);
  static_assert(((ymd{1000y, January, 1d} -= years{1}) -= months{1})
		== December/1d/998y);

  static_assert(!ymd{1000y, February, 30d}.ok());

  static_assert(June/1d/1977y == June/1d/1977y);
  static_assert(June/1d/1977y != June/1d/1987y);
  static_assert(May/15d/1950y <=> May/15d/1950y == std::strong_ordering::equal);
  static_assert(May/15d/1950y <=> May/14d/1950y == std::strong_ordering::greater);
  static_assert(April/15d/1950y <=> May/14d/1950y == std::strong_ordering::less);

  static_assert(January/1d/1900y + months{13} == February/1d/1901y);
  static_assert(months{13} + January/1d/1900y == February/1d/1901y);
  static_assert(January/1d/1900y + years{1} == January/1d/1901y);
  static_assert(years{1} + January/1d/1900y == January/1d/1901y);
  static_assert(January/1d/1900y - months{13} == December/1d/1898y);
  static_assert(January/1d/1900y - years{1} == January/1d/1899y);

  // N.B. unix seems to be a macro somewhere!
  constexpr ymd myunix = 1970y/1/1;
  static_assert(myunix.ok());
  static_assert(myunix.year() == year{1970});
  static_assert(myunix.month() == January);
  static_assert(myunix.day() == day{1});
  static_assert(sys_days(myunix).time_since_epoch() == days{0});
  //static_assert(local_days(myunix) == time_point_cast<days>(days{0}));

  static_assert(sys_days{August/20d/2020y}.time_since_epoch() == days{18494});

  static_assert(ymd{sys_days{2017y/January/0}}  == 2016y/December/31);
  static_assert(ymd{sys_days{2017y/January/31}} == 2017y/January/31);
  static_assert(ymd{sys_days{2017y/January/32}} == 2017y/February/1);
  static_assert(ymd{sys_days{2017y/33/59 + months{0}}} == 2019y/10/29);

  static_assert(ymd{local_days{2017y/January/0}}  == 2016y/December/31);
  static_assert(ymd{local_days{2017y/January/31}} == 2017y/January/31);
  static_assert(ymd{local_days{2017y/January/32}} == 2017y/February/1);
  static_assert(ymd{local_days{2017y/33/59 + months{0}}} == 2019y/10/29);

  static_assert((2000y/February/29d).ok());
  static_assert(!(2001y/February/29d).ok());
  static_assert(!(2100y/February/29d).ok());
  static_assert(!(1999y/February/29d).ok());
}
