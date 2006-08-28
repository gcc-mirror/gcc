// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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
