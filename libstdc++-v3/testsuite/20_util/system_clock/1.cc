// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-time "" }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// 20.8.5 Clocks [time.clock]

#include <chrono>

// 20.8.5.1 system_clock [time.clock.system]
int
main()
{
  using namespace std::chrono;

  system_clock::time_point t1 = system_clock::now();
  bool is_steady = system_clock::is_steady;
  is_steady = is_steady; // suppress unused warning
  std::time_t t2 = system_clock::to_time_t(t1);
  system_clock::time_point t3 = system_clock::from_time_t(t2);
  t3 = t3; // suppress unused warning
  
  return 0;
}
