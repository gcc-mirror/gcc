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

// Class template day [time.cal.day]

#include <chrono>

constexpr void
constexpr_day()
{
  using namespace std::chrono;

  day dd{};
  ++(++dd);
  dd++;
  --(--dd);
  dd--;
  dd += days{3};
  dd -= days{3};

  static_assert(day{1} + days{2} == day{3});
  static_assert(days{2} + day{1} == day{3});
  static_assert(day{3} - day{1} == days{2});
  static_assert(day{3} - days{1} == day{2});

  static_assert(++day{4} == day{5});
  static_assert(day{4}++ == day{4});
  static_assert(--day{4} == day{3});
  static_assert(day{4}-- == day{4});
  static_assert((day{4} += days{3}) == day{7});
  static_assert((day{4} -= days{3}) == day{1});

  static_assert(!day{}.ok());
  static_assert(day{1}.ok());
  static_assert(day{31}.ok());
  static_assert(!day{32}.ok());

  static_assert(unsigned{day{7}} == 7);

  static_assert(!(day{0} == day{1}));
  static_assert( (day{0} != day{2}));
  static_assert( (day{0} <  day{3}));
  static_assert(!(day{0} >  day{4}));
  static_assert( (day{0} <= day{5}));
  static_assert(!(day{0} >= day{6}));

  static_assert(day{0} <=> day{1} == std::strong_ordering::less);
  static_assert(day{3} <=> day{3} == std::strong_ordering::equal);
  static_assert(day{5} <=> day{2} == std::strong_ordering::greater);
}
