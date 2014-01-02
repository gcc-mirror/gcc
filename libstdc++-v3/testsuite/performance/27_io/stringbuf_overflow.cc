// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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


#include <sstream>
#include <testsuite_performance.h>

// libstdc++/16401 ostringstream in gcc 3.4.x very slow for big data
void test01()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  for(unsigned n = 10000; n <= 10000000; n *= 10)
    {
      ostringstream oss;
      oss << "size = " << n; 

      ostringstream str;
      start_counters(time, resource);  
      for(unsigned i = 0; i < n; ++i)
	str << 'a';
      stop_counters(time, resource);

      report_performance(__FILE__, oss.str(), time, resource);
      clear_counters(time, resource);
    }
}

int main()
{
  test01();
  return 0;
}
