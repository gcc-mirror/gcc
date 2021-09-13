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

// Class template day [time.cal.weekday]

#include <chrono>

constexpr void
constexpr_weekday()
{
  using namespace std::chrono;

  weekday dwd{};
  ++dwd;
  dwd++;
  --dwd;
  dwd--;
  dwd += days{3};
  dwd -= days{3};

  static_assert(weekday{3}[2].weekday() == weekday{3});
  static_assert(weekday{3}[last].weekday() == weekday{3});

  static_assert(weekday{sys_days{1900y/January/1}} == Monday);
  static_assert(weekday{sys_days{1970y/January/1}} == Thursday);
  static_assert(weekday{sys_days{2020y/August/21}} == Friday);

  static_assert(weekday{local_days{1900y/January/1}} == Monday);
  static_assert(weekday{local_days{1970y/January/1}} == Thursday);
  static_assert(weekday{local_days{2020y/August/21}} == Friday);

  static_assert(++weekday{3} == weekday{4});
  static_assert(weekday{3}++ == weekday{3});
  static_assert(--weekday{3} == weekday{2});
  static_assert(weekday{3}-- == weekday{3});
  static_assert((weekday{3} += days{3}) == weekday{6});
  static_assert((weekday{3} -= days{3}) == weekday{0});

  static_assert(Monday + days{7000} == Monday);
  static_assert(Monday + days{-7000} == Monday);
  static_assert(days{7001} + Monday == Tuesday);
  static_assert(days{-7001} + Monday == Sunday);
  static_assert(Monday - days{7000} == Monday);
  static_assert(Monday - days{-7000} == Monday);
  static_assert(Monday - days{7001} == Sunday);

  static_assert([] {
    constexpr unsigned diff_tbl[7][7]
      = { { 0, 6, 5, 4, 3, 2, 1},
	  { 1, 0, 6, 5, 4, 3, 2},
	  { 2, 1, 0, 6, 5, 4, 3},
	  { 3, 2, 1, 0, 6, 5, 4},
	  { 4, 3, 2, 1, 0, 6, 5},
	  { 5, 4, 3, 2, 1, 0, 6},
	  { 6, 5, 4, 3, 2, 1, 0} };
    for (unsigned x = 0; x < 7; x++)
      for (unsigned y = 0; y < 7; y++)
	{
	  if (weekday{x} - weekday{y} != days{diff_tbl[x][y]})
	    return false;
	  if (weekday{x} - days{diff_tbl[x][y]} != weekday{y})
	    return false;
	  if (weekday{x} != weekday{y} + days{diff_tbl[x][y]})
	    return false;
	  if (weekday{x} != days{diff_tbl[x][y]} + weekday{y})
	    return false;
	}
    return true;
  }());

  static_assert(Sunday.c_encoding() == 0);
  static_assert(Sunday.iso_encoding() == 7);
  static_assert(Monday.c_encoding() == 1);
  static_assert(Monday.iso_encoding() == 1);

  static_assert(!weekday{127}.ok());
  static_assert(weekday{0}.ok());
  static_assert(weekday{6}.ok());
  static_assert(weekday{7}.ok()); // Ctor wraps 7 to 0.
  static_assert(!weekday{8}.ok());

  static_assert(weekday{7} == weekday{0});
  static_assert(!(weekday{0} == weekday{1}));
  static_assert( (weekday{0} != weekday{2}));
}
