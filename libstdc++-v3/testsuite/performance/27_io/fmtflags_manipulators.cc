// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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


#include <cstdlib>
#include <sstream>
#include <testsuite_performance.h>

// libstdc++/14078
int main(int argc, char** argv)
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  int iters = 50000000;
  if (argc > 1)
    iters = atoi(argv[1]);
  
  ostringstream os_s, os_m;

  // setf
  start_counters(time, resource);
  for (int i = 0; i < iters; ++i)
    {
      os_s.setf(ios_base::uppercase);
      os_s.unsetf(ios_base::uppercase);
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "setf", time, resource);
  clear_counters(time, resource);

  // manipulator
  start_counters(time, resource);
  for (int i = 0; i < iters; ++i)
    {
      os_m << uppercase;
      os_m << nouppercase;
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "manipulator", time, resource);

  return 0;
}
