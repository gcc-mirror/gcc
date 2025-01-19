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

// Class template day [time.cal.year]

#include <chrono>

constexpr void
constexpr_year()
{
  using namespace std::chrono;

  year dy{};
  ++(++dy);
  dy++;
  --(--dy);
  dy--;
  dy += years{3};
  dy -= years{3};

  static_assert(++year{4} == year{5});
  static_assert(year{4}++ == year{4});
  static_assert(--year{4} == year{3});
  static_assert(year{4}-- == year{4});
  static_assert((year{4} += years{3}) == year{7});
  static_assert((year{4} -= years{3}) == year{1});

  static_assert(year{3} + years{7} == year{10});
  static_assert(years{3} + year{7} == year{10});
  static_assert(year{3} - years{7} == year{-4});
  static_assert(year{10} - year{30} == years{-20});

  const auto my = -dy;
  const auto py = +dy;

  static_assert((-year{1066} == year{-1066}));
  static_assert((-year{-332} == year{332}));
  static_assert((+year{1066} == year{1066}));
  static_assert((+year{-332} == year{-332}));

  year::min();
  year::max();

  static_assert(year{-12345}.ok());
  static_assert(year{1}.ok());
  static_assert(year{12}.ok());
  static_assert(year{13}.ok());

  static_assert(int{year{-42}} == -42);

  static_assert(!(year{0} == year{1}));
  static_assert( (year{0} != year{2}));
  static_assert( (year{0} <  year{3}));
  static_assert(!(year{0} >  year{4}));
  static_assert( (year{0} <= year{5}));
  static_assert(!(year{0} >= year{6}));

  static_assert(year{10} <=> year{11} == std::strong_ordering::less);
  static_assert(year{13} <=> year{13} == std::strong_ordering::equal);
  static_assert(year{15} <=> year{12} == std::strong_ordering::greater);

  static_assert( year{400}.is_leap());
  static_assert( year{1984}.is_leap());
  static_assert(!year{1}.is_leap());
  static_assert( year{1600}.is_leap());
  static_assert(!year{3000}.is_leap());
  static_assert(!year{2019}.is_leap());
}
