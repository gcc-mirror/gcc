// -*- C++ -*-

// Copyright (C) 2018-2019 Free Software Foundation, Inc.

// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 3, or (at
// your option) any later version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


// A clock that ticks at a third of the speed of system_clock that can be used
// to ensure that functions with timeouts don't erroneously return early.

#include <chrono>

struct slow_clock
{
  using rep = std::chrono::system_clock::rep;
  using period = std::chrono::system_clock::period;
  using duration = std::chrono::system_clock::duration;
  using time_point = std::chrono::time_point<slow_clock, duration>;
  static constexpr bool is_steady = false;

  static time_point now()
  {
    auto real = std::chrono::system_clock::now();
    return time_point{real.time_since_epoch() / 3};
  }
};
