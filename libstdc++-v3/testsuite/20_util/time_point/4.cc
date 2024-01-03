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

// 20.8.4 Class template time_point [time.point]

#include <chrono>

constexpr bool
test_time_point_increment_ops()
{
  using namespace std::chrono;
  bool ok = true;
  time_point<local_t, seconds> tp(seconds(1));

  ok &= tp.time_since_epoch() == 1s;
  ok &= (tp++).time_since_epoch() == 1s;
  ok &= tp.time_since_epoch() == 2s;
  ok &= (++tp).time_since_epoch() == 3s;
  ok &= (tp--).time_since_epoch() == 3s;
  ok &= tp.time_since_epoch() == 2s;
  ok &= (--tp).time_since_epoch() == 1s;
  ok &= tp.time_since_epoch() == 1s;
  return ok;
}
static_assert(test_time_point_increment_ops());
