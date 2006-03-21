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
