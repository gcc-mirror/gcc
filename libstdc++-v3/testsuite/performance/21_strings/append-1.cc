 // Copyright (C) 2003-2025 Free Software Foundation, Inc.
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


#include <ctime>
#include <iostream>
#include <string>
#include <testsuite_performance.h>

using namespace std;

void
test_append_char(int how_much)
{
  string buf; // no preallocation
  for (int i = 0; i < how_much; ++i)
    buf.append(static_cast<string::size_type>(1) , 'x');
}

void
test_append_string(int how_much)
{
  string s(static_cast<string::size_type>(1) , 'x');
  string buf; // no preallocation
  for (int i = 0; i < how_much; ++i)
    buf.append(s);
}

void 
run_benchmark1(int how_much)
{
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;
  start_counters(time, resource);
  test_append_char(how_much);
  stop_counters(time, resource);
  report_performance(__FILE__, "char", time, resource);
}

void 
run_benchmark2(int how_much)
{
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;
  start_counters(time, resource);
  test_append_string(how_much);
  stop_counters(time, resource);
  report_performance(__FILE__, "string", time, resource);
}

// libstdc++/5380
// libstdc++/4960
int main()
{
  run_benchmark1(100000);
  run_benchmark2(100000);
  run_benchmark1(1000000);
  run_benchmark2(1000000);
  run_benchmark1(10000000);
  run_benchmark2(10000000);
}
