 // Copyright (C) 2003 Free Software Foundation, Inc.
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
