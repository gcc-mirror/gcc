 // Copyright (C) 2004 Free Software Foundation, Inc.
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
