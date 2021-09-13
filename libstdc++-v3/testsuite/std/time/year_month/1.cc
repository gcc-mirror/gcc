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

// Class template year_month [time.cal.year_month]

#include <chrono>

constexpr void
constexpr_year_month()
{
  using namespace std::chrono;
  using ym = year_month;

  ym ym0 = 2015y/April;
  ym0 += years{100};
  ym0 -= years{100};
  ym0 += months{50};
  ym0 -= months{50};

  constexpr ym ym1 = {2015y, June};
  static_assert(ym1.year() == year{2015});
  static_assert(ym1.month() == June);
  static_assert(ym1.ok());

  constexpr ym ym2 = {2016y, May};
  static_assert(ym2.year() == year{2016});
  static_assert(ym2.month() == May);
  static_assert(ym2.ok());

  static_assert(ym1 == ym1);
  static_assert(ym1 != ym2);
  static_assert(ym1 < ym2);
  static_assert(ym1 <= ym2);
  static_assert(ym2 > ym1);
  static_assert(ym2 >= ym2);

  static_assert(ym1 <=> ym1 == std::strong_ordering::equal);
  static_assert(ym1 <=> ym2 == std::strong_ordering::less);
  static_assert(ym2 <=> ym1 == std::strong_ordering::greater);

  static_assert(2015y/August == ym{year{2015}, August});
  static_assert(2015y/8 == ym{year{2015}, August});

  static_assert(ym1 + months{6} == 2015y/December);
  static_assert(ym1 + months{7} == 2016y/January);
  static_assert(months{24} + ym1 == 2017y/June);
  static_assert(months{25} + ym1 == 2017y/July);

  static_assert(ym1 + months{-5} == 2015y/January);
  static_assert(ym1 + months{-6} == 2014y/December);
  static_assert(ym1 + months{-24} == 2013y/June);
  static_assert(ym1 + months{-25} == 2013y/May);

  static_assert(ym1 - months{5} == 2015y/January);
  static_assert(ym1 - months{6} == 2014y/December);
  static_assert(ym1 - months{24} == 2013y/June);
  static_assert(ym1 - months{25} == 2013y/May);

  static_assert(ym2 - ym1 == months{11});
  static_assert(ym1 - ym2 == -months{11});

  static_assert(ym2 + years{1} == 2017y/May);
  static_assert(ym2 + years{-1} == 2015y/May);
  static_assert(ym2 - years{1} == 2015y/May);

  static_assert(2017y/33 + months{0} == 2019y/9);

  static_assert(2010y/January + months{-12} == 2009y/January);

  static_assert(2010y/month{0} + months{-1} == 2009y/November);
  static_assert(2010y/month{0} + months{0} == 2009y/December);
  static_assert(2010y/month{0} + months{1} == 2010y/January);
  static_assert(2010y/month{0} + months{2} == 2010y/February);
  static_assert(2010y/month{0} + months{11} == 2010y/November);
  static_assert(2010y/month{0} + months{12} == 2010y/December);
  static_assert(2010y/month{0} + months{13} == 2011y/January);

  static_assert(months{-1} + 2010y/month{37} == 2012y/December);
  static_assert(months{0} + 2010y/month{37} == 2013y/January);
  static_assert(months{1} + 2010y/month{37} == 2013y/February);
}
