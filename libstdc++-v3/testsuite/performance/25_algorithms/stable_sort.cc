// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <vector>
#include <algorithm>
#include <testsuite_performance.h>

int main()
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const int max_size = 10000000;

  std::vector<int> v(max_size);

  for (int i = 0; i < max_size; ++i)
    v[i] = -i;

  start_counters(time, resource);
  std::stable_sort(v.begin(), v.end());
  stop_counters(time, resource);

  report_performance(__FILE__, "reverse", time, resource);
  clear_counters(time, resource);

  for (int i = 0; i < max_size; ++i)
    v[i] = i;

  start_counters(time, resource);
  std::stable_sort(v.begin(), v.end());
  stop_counters(time, resource);

  report_performance(__FILE__, "forwards", time, resource);
  clear_counters(time, resource);

  // a simple psuedo-random series which does not rely on rand() and friends
  v[0] = 0;
  for (int i = 1; i < max_size; ++i)
    v[i] = (v[i-1] + 110211473) * 745988807;

  start_counters(time, resource);
  std::stable_sort(v.begin(), v.end());
  stop_counters(time, resource);

  report_performance(__FILE__, "random", time, resource);

  return 0;
}
