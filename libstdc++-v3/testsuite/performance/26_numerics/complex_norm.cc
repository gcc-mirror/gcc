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

#include <complex>
#include <testsuite_performance.h>

// based on libstdc++/5730, use --fast-math
int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const int iterations = 2000;

  typedef complex<double> complex_type;
  complex_type u[2048];

  for (int i = 0; i < 2048; ++i)
    u[i] = 1.0;

  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i) 
    {
      complex_type * p = u;
      for (int j = 0; j < 2048; ++j) 
	{
	  double u2 = norm(*p);
	  double t = u2 * 0.1;
	  *p *= complex_type(cos(t), sin(t));
	  ++p;
	}
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "norm", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i) 
    {
      complex_type * p = u;
      for (int j = 0; j < 2048; ++j) 
	{
	  // Shouldn't be slower than the above.
	  double ur = real(*p); 
	  double ui = imag(*p);
	  double u2 = ur * ur + ui * ui;
	  double t = u2 * 0.1;
	  *p *= complex_type(cos(t), sin(t));
	  ++p;
	}
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}

