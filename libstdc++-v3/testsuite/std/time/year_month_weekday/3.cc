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

// PR libstdc++/97613
// Test year_month_weekday to sys_days conversion for extreme values of index().

#include <chrono>

void
test01()
{
  using namespace std::chrono;
  using ymd = year_month_day;

  static_assert(ymd{sys_days{2020y/January/Sunday[0]}} == 2019y/December/29);
  static_assert(ymd{sys_days{2020y/January/Monday[0]}} == 2019y/December/30);
  static_assert(ymd{sys_days{2020y/January/Tuesday[0]}} == 2019y/December/31);
  static_assert(ymd{sys_days{2020y/January/Wednesday[0]}} == 2019y/December/25);
  static_assert(ymd{sys_days{2020y/January/Thursday[0]}} == 2019y/December/26);
  static_assert(ymd{sys_days{2020y/January/Friday[0]}} == 2019y/December/27);
  static_assert(ymd{sys_days{2020y/January/Saturday[0]}} == 2019y/December/28);

  static_assert((2020y).is_leap());
  static_assert(ymd{sys_days{2020y/March/Sunday[0]}} == 2020y/February/23);
  static_assert(ymd{sys_days{2020y/March/Monday[0]}} == 2020y/February/24);
  static_assert(ymd{sys_days{2020y/March/Tuesday[0]}} == 2020y/February/25);
  static_assert(ymd{sys_days{2020y/March/Wednesday[0]}} == 2020y/February/26);
  static_assert(ymd{sys_days{2020y/March/Thursday[0]}} == 2020y/February/27);
  static_assert(ymd{sys_days{2020y/March/Friday[0]}} == 2020y/February/28);
  static_assert(ymd{sys_days{2020y/March/Saturday[0]}} == 2020y/February/29);

  static_assert(!(2019y).is_leap());
  static_assert(ymd{sys_days{2019y/March/Sunday[0]}} == 2019y/February/24);
  static_assert(ymd{sys_days{2019y/March/Monday[0]}} == 2019y/February/25);
  static_assert(ymd{sys_days{2019y/March/Tuesday[0]}} == 2019y/February/26);
  static_assert(ymd{sys_days{2019y/March/Wednesday[0]}} == 2019y/February/27);
  static_assert(ymd{sys_days{2019y/March/Thursday[0]}} == 2019y/February/28);
  static_assert(ymd{sys_days{2019y/March/Friday[0]}} == 2019y/February/22);
  static_assert(ymd{sys_days{2019y/March/Saturday[0]}} == 2019y/February/23);

  static_assert(ymd{sys_days{2020y/December/Sunday[5]}} == 2021y/January/3);
  static_assert(ymd{sys_days{2020y/December/Monday[5]}} == 2021y/January/4);
  static_assert(ymd{sys_days{2020y/December/Tuesday[5]}} == 2020y/December/29);
  static_assert(ymd{sys_days{2020y/December/Wednesday[5]}} == 2020y/December/30);
  static_assert(ymd{sys_days{2020y/December/Thursday[5]}} == 2020y/December/31);
  static_assert(ymd{sys_days{2020y/December/Friday[5]}} == 2021y/January/1);
  static_assert(ymd{sys_days{2020y/December/Saturday[5]}} == 2021y/January/2);
}
