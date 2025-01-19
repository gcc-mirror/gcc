// { dg-do run { target c++20 } }
// { dg-additional-options "-DSTART_DAY=-50000 -DSTART_YMD=1833y/February/8d -DEND_YMD=2106y/November/24d" { target simulator } }

// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// Class year_month_day [time.cal.year_month_day]

#include <chrono>
#include <testsuite_hooks.h>

// Slow but very clear way of advancing one day.
constexpr void
advance(std::chrono::year_month_day& ymd) noexcept {

  using namespace std::chrono;

  auto y = ymd.year();
  auto m = ymd.month();
  auto d = ymd.day();

  if (d != year_month_day_last{year{y}, month_day_last{m}}.day())
    ++d;
  else {
    d = day{1};
    if (m != December)
      ++m;
    else {
      m = January;
      ++y;
    }
  }
  ymd = year_month_day{y, m, d};
}

void test01()
{
  using namespace std::chrono;

#ifdef START_DAY
  auto n   = days{START_DAY};
  auto ymd = START_YMD;
#else
  // [-32767y/January/1d, 32767y/December/31d] maps to [-12687428, 11248737]

  auto n   = days{-12687428};
  auto ymd = -32767y/January/1d;
#define END_YMD 32767y/December/31d
#endif

  while (ymd < END_YMD) {
    VERIFY( static_cast<sys_days>(ymd) == sys_days{n} );
    ++n;
    advance(ymd);
  }
  // One more for ymd = 32767y/December/31d and n = 11248737.
  VERIFY( sys_days{days{11248737}} == static_cast<sys_days>(32767y/December/31d) );
}

int main()
{
  test01();
  return 0;
}
