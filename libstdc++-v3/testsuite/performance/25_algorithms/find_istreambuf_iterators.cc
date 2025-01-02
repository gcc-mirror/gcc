// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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


#include <iterator>
#include <sstream>
#include <algorithm>
#include <testsuite_performance.h>

// libstdc++/25482
int main()
{
  using namespace std;
  using namespace __gnu_test;

  typedef istreambuf_iterator<char> in_iterator_type;

  istringstream iss("a0000b1111c2222d3333e4444f5555g6666h7777i8888j9999"
		    "k0000l1111m2222n3333o4444p5555q6666r7777s8888t9999");

  in_iterator_type beg(iss);
  in_iterator_type end;

  time_counter time;
  resource_counter resource;

  start_counters(time, resource);
  for (unsigned i = 0; i < 1000000; ++i)
    {
      for (char c = 'a'; c < 'u'; ++c)
	{
	  find(beg, end, c);
	  iss.seekg(0);
	}
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
