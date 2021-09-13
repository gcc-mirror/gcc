// Copyright (C) 2006-2021 Free Software Foundation, Inc.
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
  typedef ostreambuf_iterator<char> out_iterator_type;

  time_counter time;
  resource_counter resource;

  const char data[] = "Contrappunto dialettico alla mente";

  // istreambuf iterators -> ostreambuf iterator
  {
    istringstream iss(data);
    in_iterator_type beg(iss);
    in_iterator_type end;
    
    ostringstream oss;
    out_iterator_type out(oss);

    start_counters(time, resource);
    for (unsigned i = 0; i < 10000000; ++i)
      {
	copy(beg, end, out);
	iss.seekg(0);
	oss.seekp(0);
      }
    stop_counters(time, resource);
    report_performance(__FILE__, "isb iters -> osb iter", time, resource);
    clear_counters(time, resource);
  }

  // char array -> ostreambuf iterator
  {
    const char* beg = data;
    const char* end = data + sizeof(data) - 1;

    ostringstream oss;
    out_iterator_type out(oss);

    start_counters(time, resource);
    for (unsigned i = 0; i < 10000000; ++i)
      {
	copy(beg, end, out);
	oss.seekp(0);
      }
    stop_counters(time, resource);
    report_performance(__FILE__, "pointers  -> osb iter", time, resource);
    clear_counters(time, resource);
  }

  // istreambuf iterators -> char array
  {
    istringstream iss(data);
    in_iterator_type beg(iss);
    in_iterator_type end;

    char out[sizeof(data)];

    start_counters(time, resource);
    for (unsigned i = 0; i < 10000000; ++i)
      {
	copy(beg, end, out);
	iss.seekg(0);
      }
    stop_counters(time, resource);
    report_performance(__FILE__, "isb iters -> pointer", time, resource);
    clear_counters(time, resource);
  }

  return 0;
}
