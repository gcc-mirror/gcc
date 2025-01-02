// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

// P0972R0 <chrono> zero(), min(), and max() should be noexcept

#include <chrono>

using namespace std;

using tp1 = chrono::system_clock::time_point;
static_assert( noexcept(tp1::min()), "time_point::min()" );
static_assert( noexcept(tp1::max()), "time_point::max()" );

struct Clock {
  using rep = int;
  using period = ratio<1, 24>;
  using duration = chrono::duration<rep, period>;
  using time_point = chrono::time_point<Clock>;
  static constexpr bool is_steady = false;
  static time_point now() noexcept;
};

using tp2 = Clock::time_point;
static_assert( noexcept(tp2::min()), "time_point::min()" );
static_assert( noexcept(tp2::max()), "time_point::max()" );

using tp3 = chrono::time_point<Clock, chrono::duration<long, ratio<10>>>;
static_assert( noexcept(tp3::min()), "time_point::min()" );
static_assert( noexcept(tp3::max()), "time_point::max()" );
