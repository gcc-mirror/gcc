// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

#include <unistd.h>
#include <fstream>
#include <testsuite_performance.h>

// libstdc++/10672
int main() 
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const int iterations = 300000;

  fstream s("tmp_perf_seek", ios::binary | ios::in | ios::out | ios::trunc);
  if (s.good())
    {
      start_counters(time, resource);
      for (int i = 0; i < iterations; i++) 
	{
	  s.seekp(0);
	  s.write((char *) & i, sizeof(int));
	  s.seekp(sizeof(int));
	  s.write((char *) & i, sizeof(int));
	}
      stop_counters(time, resource);
      report_performance(__FILE__, "", time, resource);
    }

  unlink("tmp_perf_seek");
  return 0;
}
