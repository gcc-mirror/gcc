// Copyright (C) 2005 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <vector>
#include <set>
#include <sstream>
#include <testsuite_performance.h>

static const unsigned max_size = 1000000; // avoid excessive swap file use!
static const unsigned iterations = 10;    // make results less random while
static const unsigned step = 50000;       // keeping the total time reasonable

// libstdc++/19433
int main()
{
  using namespace std;
  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  typedef set<unsigned>  the_set;

  vector<unsigned> v(max_size, 0);
  for (unsigned i = 0; i != max_size; ++i)
    v[i] = i; // initialize sorted array

  for (unsigned count = step; count <= max_size; count += step)
    {
      ostringstream oss;
      oss << count;

      start_counters(time, resource);
      for (unsigned i = 0; i != iterations; ++i)
	{
	  the_set test_set;
	  the_set::iterator iter = test_set.end();

	  // each insert in amortized constant time (Table 69)
	  for (unsigned j = 0; j != count; ++j)
	    iter = test_set.insert(iter, v[j]);
	}
      stop_counters(time, resource);
      report_performance(__FILE__, oss.str(), time, resource);
      clear_counters(time, resource);
    }
}
