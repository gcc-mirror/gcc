// Copyright (C) 2004-2021 Free Software Foundation, Inc.
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


#define DISABLE_ITERATOR_DEBUG 1

#include<cstdlib>
#include<vector>
#include<algorithm>

#include<sstream>
#include<testsuite_performance.h>
#include<testsuite_iterators.h>

using namespace std;
using namespace __gnu_test;

const int length = 10000000;
const int match_length = 3;
int ary[length];

int
main(void)
{
  time_counter time;
  resource_counter resource;
  int match = rand() % (match_length - 1);
  for(int i = 0; i < length; i++)
    {
      ary[i] = (match != 0) ? 1 : 0;
      if(--match < 0) match = rand() % (match_length - 1);
    }
  __gnu_test::test_container<int, forward_iterator_wrapper> fcon(ary, ary + length);
  start_counters(time, resource);
  for(int i = 0; i < 100; i++)
    search_n(fcon.begin(), fcon.end(), 10, 1);
  stop_counters(time, resource);
  report_performance(__FILE__, "forward iterator", time, resource);
  clear_counters(time, resource);

  __gnu_test::test_container<int, random_access_iterator_wrapper> rcon(ary, ary + length);
  start_counters(time, resource);
  for(int i = 0; i < 100; i++)
    search_n(rcon.begin(), rcon.end(), 10, 1);
  stop_counters(time, resource);
  report_performance(__FILE__, "random access iterator", time, resource);
  clear_counters(time, resource);
}

