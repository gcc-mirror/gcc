// { dg-do run { target c++20 } }

// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

// Class year [time.cal.year_month_day]

#include <chrono>
#include <testsuite_hooks.h>

// Slow but clear test for leap year.
constexpr bool
is_leap_year(const std::chrono::year& y) noexcept
{
  const int n = static_cast<int>(y);
  return n % 4 == 0 && (n % 100 != 0 || n % 400 == 0);
}

void test01()
{
  using namespace std::chrono;

  year y{-32767};
  while (y < year{32767}) {
    VERIFY( y.is_leap() ==  is_leap_year(y) );
    ++y;
  }

  // One more for y = 32767.
  VERIFY( year{32767}.is_leap() == is_leap_year(year{32767}) );
}

int main()
{
  test01();
  return 0;
}
