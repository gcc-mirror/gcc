// 2003-03-01 gp dot bolton at computer dot org

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

#include <map>
#include <testsuite_hooks.h>
#include <testsuite_performance.h>

static void create_and_fill(const unsigned int n)
{
  typedef std::map<int, int>  Map;
  Map                         m;
  bool test __attribute__((unused)) = true;
  
  for (unsigned int i = 0; i < n; ++i)
    m[i] = i;
  VERIFY( m.size() == n );
}

// http://gcc.gnu.org/ml/libstdc++/2003-03/msg00000.html
int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const int iterations = 100000000;
  
  start_counters(time, resource);
  for (int i = 0; i < iterations; ++i)
    create_and_fill( 0 );
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
