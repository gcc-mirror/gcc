// Copyright (C) 2012 Free Software Foundation, Inc.
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

// { dg-options "-std=c++11" }

#include <unordered_set>
#include <testsuite_performance.h>

int main()
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const int sz = 10000000;

  std::unordered_set<int> s;
  start_counters(time, resource);

  for (int i = 0; i != sz ; ++i)
    s.insert(i);

  stop_counters(time, resource);
  report_performance(__FILE__, "unordered_set 10000000 insertions",
		     time, resource);
  return 0;
}
