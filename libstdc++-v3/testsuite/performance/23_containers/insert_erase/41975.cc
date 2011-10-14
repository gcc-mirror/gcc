// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

#include <cassert>
#include <unordered_set>
#include <testsuite_performance.h>

int main()
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  start_counters(time, resource);

  std::unordered_set<int> us;
  for (int i = 0; i != 5000000; ++i)
    us.insert(i);

  stop_counters(time, resource);
  report_performance(__FILE__, "Container generation", time, resource);

  start_counters(time, resource);

  for (int j = 100; j != 0; --j)
    {
      auto it = us.begin();
      while (it != us.end())
	{
	  if ((*it % j) == 0)
	    it = us.erase(it);
	  else
	    ++it;
	}
    }

  stop_counters(time, resource);
  report_performance(__FILE__, "Container erase", time, resource);

  start_counters(time, resource);

  us.insert(0);

  for (int i = 0; i != 500; ++i)
    {
      auto it = us.begin();
      ++it;
      assert( it == us.end() );
    }

  stop_counters(time, resource);
  report_performance(__FILE__, "Container iteration", time, resource);

  return 0;
}
