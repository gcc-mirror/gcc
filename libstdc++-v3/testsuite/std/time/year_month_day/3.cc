// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2021 Free Software Foundation, Inc.
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

  // [-12687428, 11248737] maps to [-32767y/January/1d, 32767y/December/31d]

  auto n   = days{-12687428};
  auto ymd = -32767y/January/1d;
  while (n < days{11248737}) {
    VERIFY( year_month_day{sys_days{n}} == ymd );
    ++n;
    advance(ymd);
  }
  // One more for n = 11248737 and ymd = 32767y/December/31d
  VERIFY( 32767y/December/31d == year_month_day{sys_days{days{11248737}}} );
}

int main()
{
  test01();
  return 0;
}
