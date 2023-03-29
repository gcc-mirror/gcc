 // Copyright (C) 2004-2023 Free Software Foundation, Inc.
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


#include <string>
#include <testsuite_performance.h>

void
test_pair(const std::string& s, const std::string& f, int n)
{
  std::string::size_type sz = 0;

  for (int i = 0; i < n; ++i)
    sz = s.find(f);
}

int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const unsigned int iterations = 2000000;

  string s, f;
  s = "aabbaabbaaxd adbffdadgaxaabbbddhatyaaaabbbaabbaabbcsy";
  f = "aabbaabbc";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "1", time, resource);
  clear_counters(time, resource);

  f = "aabbb";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "2", time, resource);
  clear_counters(time, resource);

  f = "xd";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "3", time, resource);
  clear_counters(time, resource);

  s = "dhruv is a very very good boy ;-)";
  f = "very";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "4", time, resource);
  clear_counters(time, resource);

  f = "bad";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "5", time, resource);
  clear_counters(time, resource);

  f = "extra irritating";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "6", time, resource);
  clear_counters(time, resource);

  s = "this is a very this is a very this is a verty this is a very "
      "this is a very long sentence";
  f = "this is a very long sentence";
  start_counters(time, resource);
  test_pair(s, f, iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "7", time, resource);
  clear_counters(time, resource);

  return 0;
}
