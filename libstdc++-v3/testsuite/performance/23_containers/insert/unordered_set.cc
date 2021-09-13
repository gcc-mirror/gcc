// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <testsuite_performance.h>
#include <sstream>
#ifdef _USE_TR1
#  include <tr1/unordered_set>
using namespace std::tr1;
const char* ns = "std::tr1::";
#else
#  include<unordered_set>
using namespace std;
const char* ns = "std::";
#endif

int main()
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const int sz = 10000000;

  unordered_set<int> s;
  start_counters(time, resource);

  for (int i = 0; i != sz ; ++i)
    s.insert(i);

  stop_counters(time, resource);
  std::ostringstream ostr;
  ostr << ns << "unordered_set " << sz << " insertions";
  report_performance(__FILE__, ostr.str().c_str(),
		     time, resource);
  return 0;
}
