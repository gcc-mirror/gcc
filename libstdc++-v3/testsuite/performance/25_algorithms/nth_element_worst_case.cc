// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

  const int max_size = 8192;

  std::vector<int> v[max_size];

  for (int i = 0; i < max_size; ++i)
    {
      for (int j = 0; j < i; j += 4)
	{
	  v[i].push_back(j / 2);
	  v[i].push_back((i - 2) - (j / 2));
	}

      for (int j = 1; j < i; j += 2)
	v[i].push_back(j);
    }

  start_counters(time, resource);
  for (int i = 0; i < max_size; ++i)
    std::nth_element(v[i].begin(), v[i].begin() + i, v[i].end());
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
