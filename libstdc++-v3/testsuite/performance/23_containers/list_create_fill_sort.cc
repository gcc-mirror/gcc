// 2003-07-07 gp dot bolton at computer dot org

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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

#include <list>
#include <testsuite_hooks.h>
#include <testsuite_performance.h>


static void create_and_fill_and_sort(const unsigned int n)
{
  typedef std::list<int>  List;
  List                    l;
  
  for (unsigned int i = 0; i < n; ++i)
  {
      l.push_back(n - i);
  }
  l.sort();
}


int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  char comment[80];

  for (unsigned int n = 1; n <= 1000; n *= 10)
  {
      const unsigned int iterations = 10000000/n;

      start_counters(time, resource);

      for (unsigned int i = 0; i < iterations; ++i)
      {
	  create_and_fill_and_sort( n );
      }
      stop_counters(time, resource);

      sprintf(comment,"Iters: %8u  Size: %4u", iterations, n);
      report_performance(__FILE__, comment, time, resource);
  }
  return 0;
}
