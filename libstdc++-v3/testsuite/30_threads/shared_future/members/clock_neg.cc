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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <future>

struct clok
{
  // no clok::rep or clok::period defined
  using duration = std::chrono::milliseconds;
  using time_point = std::chrono::time_point<clok>;
  static constexpr bool is_steady = false;
  static time_point now();
};

void
test01()
{
  std::promise<void> p;
  std::shared_future<void> f = p.get_future();
  f.wait_until(clok::now()); // { dg-error "here" }
}

struct cloc
{
  using duration = std::chrono::milliseconds;
  using rep = duration::rep;
  using period = duration::period;
  // cloc::time_point::duration should be the same as cloc::duration:
  using time_point = std::chrono::time_point<cloc, std::chrono::seconds>;
  static constexpr bool is_steady = false;
  static time_point now();
};

void
test02()
{
  std::promise<void> p;
  std::shared_future<void> f = p.get_future();
  f.wait_until(cloc::now()); // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
