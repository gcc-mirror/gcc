// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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


#include <atomic>
#include <testsuite_performance.h>

volatile std::atomic_flag af;
volatile std::atomic_uchar ac;
volatile std::atomic_int ai;

int main()
{
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  const int n = 100000000;

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    af.test_and_set();
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_flag::test_and_set()", time, resource);

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    af.clear();
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_flag::clear()", time, resource);

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    ac |= 1;
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_uchar::operator|=(1)", time, resource);

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    ac = 0;
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_flag::operator=(0)", time, resource);

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    ai |= 1;
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_int::operator|=(1)", time, resource);

  start_counters(time, resource);
  for (int i = 0; i < n; ++i)
    ai = 0;
  stop_counters(time, resource);
  report_performance(__FILE__, "atomic_int::operator=(0)", time, resource);

  return 0;
}
