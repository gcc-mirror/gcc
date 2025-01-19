// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#include <testsuite_new_operators.h>
#include <testsuite_performance.h>

const int max_size = 10000000;
const int small_size = 200000;

void bench(size_t mem_threshold,
	   std::vector<int> revv,
	   std::vector<int> fwdv,
	   std::vector<int> rndv)
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::stable_sort(revv.begin(), revv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));
  report_performance(__FILE__, "reverse", time, resource);
  clear_counters(time, resource);

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::stable_sort(fwdv.begin(), fwdv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));
  report_performance(__FILE__, "forwards", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  std::stable_sort(rndv.begin(), rndv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));
  report_performance(__FILE__, "random", time, resource);
}

int main()
{
  using namespace __gnu_test;

  // No memory constraint.
  set_new_limit(~size_t(0));

  std::vector<int> revv(max_size);
  for (int i = 0; i < max_size; ++i)
    revv[i] = -i;

  std::vector<int> fwdv(max_size);
  for (int i = 0; i < max_size; ++i)
    fwdv[i] = i;

  // a simple pseudo-random series which does not rely on rand() and friends
  std::vector<int> rndv(max_size);
  rndv[0] = 0;
  for (int i = 1; i < max_size; ++i)
    rndv[i] = (rndv[i-1] + 110211473) * 745988807;

  time_counter time;
  resource_counter resource;

  start_counters(time, resource);
  bench(~size_t(0), revv, fwdv, rndv);
  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 / 1 memory", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  // Limit to fourth the expected size of the sorted array.
  bench(max_size * sizeof(int) / 4, revv, fwdv, rndv);
  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 / 4 memory", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  // Limit to 1/64 of range size.
  bench(max_size * sizeof(int) / 64, revv, fwdv, rndv);
  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 /64 memory", time, resource);
  clear_counters(time, resource);

  revv.resize(small_size);
  fwdv.resize(small_size);
  rndv.resize(small_size);

  start_counters(time, resource);
  // Forbid any allocation.
  bench(0, revv, fwdv, rndv);
  stop_counters(time, resource);

  report_performance(__FILE__, "bench 0 / 1 memory", time, resource);
  return 0;
}
