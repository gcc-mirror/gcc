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

// Class template day [time.cal.month]

#include <chrono>

constexpr void
constexpr_month()
{
  using namespace std::chrono;

  month dm{};
  ++(++dm);
  dm++;
  --(--dm);
  dm--;
  dm += months{3};
  dm -= months{3};

  static_assert(February + months{11} == January);
  static_assert(January + months{1200} == January);
  static_assert(January + months{1201} == February);
  static_assert(months{-1200} + January == January);
  static_assert(months{-1201} + January == December);
  static_assert(January - months{1200} == January);
  static_assert(January - months{-1200} == January);
  static_assert(January - months{1201} == December);

  static_assert(January - February == months{11});
  static_assert(February - January == months{1});
  static_assert(June - June == months{});

  static_assert(++month{4} == month{5});
  static_assert(month{4}++ == month{4});
  static_assert(--month{4} == month{3});
  static_assert(month{4}-- == month{4});
  static_assert((month{4} += months{3}) == month{7});
  static_assert((month{4} -= months{3}) == month{1});

  static_assert(!month{}.ok());
  static_assert(month{1}.ok());
  static_assert(month{12}.ok());
  static_assert(!month{13}.ok());

  static_assert(unsigned{month{7}} == 7);

  static_assert(!(month{0} == month{1}));
  static_assert( (month{0} != month{2}));
  static_assert( (month{0} <  month{3}));
  static_assert(!(month{0} >  month{4}));
  static_assert( (month{0} <= month{5}));
  static_assert(!(month{0} >= month{6}));

  static_assert(month{0} <=> month{1} == std::strong_ordering::less);
  static_assert(month{3} <=> month{3} == std::strong_ordering::equal);
  static_assert(month{5} <=> month{2} == std::strong_ordering::greater);
}
