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

#include <string>
#include <testsuite_performance.h>

void benchmark(long len)
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  
  start_counters(time, resource);
  string a("1");
  for (long i = 0; i < len; ++i)
    {
      string ss1(a);
      string ss2(ss1);
      string ss3(ss2);
      string ss4(ss3);
      string ss5(ss4);
    }
  stop_counters(time, resource);

  report_performance(__FILE__, "", time, resource);
  clear_counters(time, resource);
}

int main()
{
  benchmark(1000000);
  benchmark(10000000);
  benchmark(100000000);
  return 0;
}
