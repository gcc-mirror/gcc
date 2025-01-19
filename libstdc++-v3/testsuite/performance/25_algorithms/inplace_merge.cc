// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
#include <cmath>

#include <testsuite_new_operators.h>
#include <testsuite_performance.h>

const int max_size = 10000000;
const int small_size = 200000;
const int front_pivot_idx = 10000;
int middle_pivot_idx = max_size / 2;
int back_pivot_idx = max_size - front_pivot_idx;

void bench(int mem_threshold, int pivot_index,
	   std::vector<int> revv,
	   std::vector<int> fwdv,
	   std::vector<int> wstv,
	   std::vector<int> rndv)
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::inplace_merge(revv.begin(), revv.begin() + pivot_index, revv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));

  report_performance(__FILE__, "reverse", time, resource);
  clear_counters(time, resource);

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::inplace_merge(fwdv.begin(), fwdv.begin() + pivot_index, fwdv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));

  report_performance(__FILE__, "forward", time, resource);
  clear_counters(time, resource);

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::inplace_merge(wstv.begin(), wstv.begin() + pivot_index, wstv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));

  report_performance(__FILE__, "worst", time, resource);
  clear_counters(time, resource);

  set_new_limit(mem_threshold);

  start_counters(time, resource);
  std::inplace_merge(rndv.begin(), rndv.begin() + pivot_index, rndv.end());
  stop_counters(time, resource);

  set_new_limit(~size_t(0));
  report_performance(__FILE__, "random", time, resource);
}

void mem_bench(double mem_ratio,
	       const std::vector<int>& front_revv,
	       const std::vector<int>& middle_revv,
	       const std::vector<int>& back_revv,
	       const std::vector<int>& fwdv,
	       const std::vector<int>& front_wstv,
	       const std::vector<int>& middle_wstv,
	       const std::vector<int>& back_wstv,
	       const std::vector<int>& front_rndv,
	       const std::vector<int>& middle_rndv,
	       const std::vector<int>& back_rndv)
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  int max_mem = (int)std::ceil(front_pivot_idx * mem_ratio) * sizeof(int);
  start_counters(time, resource);
  bench(max_mem, front_pivot_idx, front_revv, fwdv, front_wstv, front_rndv);
  stop_counters(time, resource);
  report_performance(__FILE__, "front pivot", time, resource);
  clear_counters(time, resource);

  max_mem = (int)std::ceil(middle_pivot_idx * mem_ratio) * sizeof(int);
  start_counters(time, resource);
  bench(max_mem, middle_pivot_idx, middle_revv, fwdv, middle_wstv, middle_rndv);
  stop_counters(time, resource);
  report_performance(__FILE__, "middle pivot", time, resource);
  clear_counters(time, resource);

  max_mem = (int)std::ceil(front_pivot_idx * mem_ratio) * sizeof(int);
  start_counters(time, resource);
  bench(max_mem, back_pivot_idx, back_revv, fwdv, back_wstv, back_rndv);
  stop_counters(time, resource);
  report_performance(__FILE__, "back pivot", time, resource);
}

void init_reverse(std::vector<int>& v, size_t pivot_index)
{
  int val = 0;
  for (size_t i = pivot_index; i != v.size(); ++i)
    v[i] = val++;
  for (size_t i = 0; i != pivot_index; ++i)
    v[i] = val++;
}

void init_forward(std::vector<int>& v)
{
  int val = 0;
  for (size_t i = 0; i != v.size(); ++i)
    v[i] = val++;
}

void init_worst(std::vector<int>& v, size_t pivot_index)
{
  int val = 0;
  if (pivot_index + 1 > v.size() / 2)
    {
      for (size_t i = 0; i != pivot_index; val += 2, ++i)
	v[i] = val;
      val = 1;
    }
  else
    {
      for (size_t i = pivot_index; i != v.size(); val += 2, ++i)
	v[i] = val;
      val -= pivot_index * 2 + 1;
    }

  if (pivot_index + 1 > v.size() / 2)
    for (size_t i = pivot_index; i != v.size(); val += 2, ++i)
      v[i] = val;
  else
    for (size_t i = 0; i != pivot_index; val += 2, ++i)
      v[i] = val;
}

void init_random(std::vector<int>& v)
{
  // a simple pseudo-random series which does not rely on rand() and friends
  v[0] = 0;
  for (size_t i = 1; i != v.size(); ++i)
    v[i] = (v[i-1] + 110211473) * 745988807;
}

void reduce_size(std::vector<int>& front_v,
		 std::vector<int>& middle_v,
		 std::vector<int>& back_v)
{
  front_v.erase(front_v.begin() + front_pivot_idx,
		front_v.end() - back_pivot_idx);
  middle_v.erase(middle_v.begin() + small_size / 2,
		 middle_v.end() - small_size / 2);
  back_v.erase(back_v.begin() + back_pivot_idx,
	       back_v.end() - front_pivot_idx);
}

int main()
{
  using namespace __gnu_test;

  // No constraint to build vectors.
  set_new_limit(~size_t(0));

  std::vector<int> front_revv(max_size);
  init_reverse(front_revv, front_pivot_idx);

  std::vector<int> middle_revv(max_size);
  init_reverse(middle_revv, middle_pivot_idx);

  std::vector<int> back_revv(max_size);
  init_reverse(back_revv, back_pivot_idx);

  std::vector<int> fwdv(max_size);
  init_forward(fwdv);

  std::vector<int> front_wstv(max_size);
  init_worst(front_wstv, front_pivot_idx);

  std::vector<int> middle_wstv(max_size);
  init_worst(middle_wstv, middle_pivot_idx);

  std::vector<int> back_wstv(max_size);
  init_worst(back_wstv, back_pivot_idx);

  std::vector<int> front_rndv(max_size);
  init_random(front_rndv);
  std::vector<int> middle_rndv(front_rndv);
  std::vector<int> back_rndv(front_rndv);

  sort(front_rndv.begin(), front_rndv.begin() + front_pivot_idx);
  sort(front_rndv.begin() + front_pivot_idx, front_rndv.end());

  sort(middle_rndv.begin(), middle_rndv.begin() + middle_pivot_idx);
  sort(middle_rndv.begin() + middle_pivot_idx, middle_rndv.end());

  sort(back_rndv.begin(), back_rndv.begin() + back_pivot_idx);
  sort(back_rndv.begin() + back_pivot_idx, back_rndv.end());

  time_counter time;
  resource_counter resource;

  start_counters(time, resource);

  // No limit.
  mem_bench(1.0,
	    front_revv, middle_revv, back_revv,
	    fwdv,
	    front_wstv, middle_wstv, back_wstv,
	    front_rndv, middle_rndv, back_rndv);

  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 / 1 memory", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);

  // Limit to the fourth.
  mem_bench(1.0 / 4,
	    front_revv, middle_revv, back_revv,
	    fwdv,
	    front_wstv, middle_wstv, back_wstv,
	    front_rndv, middle_rndv, back_rndv);

  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 / 4 memory", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);

  // Really limit allocation.
  mem_bench(1.0 / 64,
	    front_revv, middle_revv, back_revv,
	    fwdv,
	    front_wstv, middle_wstv, back_wstv,
	    front_rndv, middle_rndv, back_rndv);

  stop_counters(time, resource);

  report_performance(__FILE__, "bench 1 /64 memory", time, resource);
  clear_counters(time, resource);

  middle_pivot_idx = small_size / 2;
  back_pivot_idx = small_size - front_pivot_idx;
  reduce_size(front_revv, middle_revv, back_revv);
  fwdv.resize(small_size);
  reduce_size(front_wstv, middle_wstv, back_wstv);
  reduce_size(front_rndv, middle_rndv, back_rndv);

  start_counters(time, resource);

  // No memory.
  mem_bench(0.0,
	    front_revv, middle_revv, back_revv,
	    fwdv,
	    front_wstv, middle_wstv, back_wstv,
	    front_rndv, middle_rndv, back_rndv);

  stop_counters(time, resource);

  report_performance(__FILE__, "bench 0 / 1 memory", time, resource);
  return 0;
}
