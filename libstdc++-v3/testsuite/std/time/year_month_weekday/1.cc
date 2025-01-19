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

// Class template day [time.cal.year_month_weekday]

#include <chrono>

constexpr void
constexpr_year_month_weekday()
{
  using namespace std::chrono;
  using ymwd = year_month_weekday;

  year_month_weekday ymwd1{};
  ymwd1 += months{9};
  ymwd1 -= months{9};
  ymwd1 += years{12};
  ymwd1 -= years{12};

  constexpr ymwd ymwd2{year{1984}, month{August},
		       weekday_indexed{Wednesday, 3}};
  static_assert(ymwd2.ok());
  static_assert(ymwd2.year() == year{1984});
  static_assert(ymwd2.month() == August);
  static_assert(ymwd2.weekday() == Wednesday);
  static_assert(ymwd2.index() == 3);
  static_assert(ymwd2.weekday_indexed() == weekday_indexed{Wednesday, 3});
  static_assert(ymwd{sys_days{ymwd2}} == ymwd2);
  static_assert(ymwd{local_days{ymwd2}} == ymwd2);

  static_assert(2015y/August/Friday[2] == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(2015y/(August/Friday[2]) == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(2015/(August/Friday[2]) == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(August/Friday[2]/2015y == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(August/Friday[2]/2015 == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});

  static_assert(January/Tuesday[2]/1900y + months{1} == February/Tuesday[2]/1900y);
  static_assert(months{1} + January/Tuesday[2]/1900y == February/Tuesday[2]/1900y);
  static_assert(January/Tuesday[2]/1900y - months{1} == December/Tuesday[2]/1899y);
  static_assert(January/Tuesday[2]/1900y + years{1} == January/Tuesday[2]/1901y);
  static_assert(years{1} + January/Tuesday[2]/1900y == January/Tuesday[2]/1901y);
  static_assert(January/Tuesday[2]/1900y - years{1} == January/Tuesday[2]/1899y);

  static_assert(January/Tuesday[1]/1900y != February/Tuesday[1]/1900y);
  static_assert(January/Tuesday[1]/1900y != January/Wednesday[1]/1900y);
  static_assert(January/Tuesday[1]/1900y != January/Tuesday[1]/1901y);
  static_assert(January/Tuesday[1]/1900y != January/Tuesday[2]/1900y);

  // N.B. unix seems to be a macro somewhere!
  constexpr ymwd myunix(local_days{days{0}});
  static_assert(myunix.ok());
  static_assert(myunix.year() == year{1970});
  static_assert(myunix.month() == January);
  static_assert(myunix.weekday() == Thursday);
  static_assert(myunix.index() == 1);
  static_assert(myunix.weekday_indexed() == weekday_indexed{Thursday, 1});
  static_assert(ymwd{sys_days{myunix}} == myunix);
  static_assert(ymwd{local_days{myunix}} == myunix);

  static_assert((2020y/August/Monday[5]).ok());
  static_assert(!(2020y/August/Tuesday[5]).ok());
}
