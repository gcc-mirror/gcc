// 2009-24-12  Paolo Carlini  <paolo.carlini@oracle.com>
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

#include <deque>
#include <vector>
#include <list>

#include <testsuite_performance.h>

int main()
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  const std::deque<int> data(3000, 3);

  std::deque<int> d(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(data.begin(), data.begin() + j, d.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "deque 2 deque", time, resource);
  clear_counters(time, resource);

  std::vector<int> v(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(data.begin(), data.begin() + j, v.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "deque 2 vector", time, resource);
  clear_counters(time, resource);

  d.assign(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(v.begin(), v.begin() + j, d.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "vector 2 deque", time, resource);
  clear_counters(time, resource);

  std::vector<char> cv(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(data.begin(), data.begin() + j, cv.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "int deque 2 char vector", time, resource);
  clear_counters(time, resource);

  d.assign(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(cv.begin(), cv.begin() + j, d.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "char vector 2 int deque", time, resource);
  clear_counters(time, resource);

  std::list<int> l(3000, 1);

  start_counters(time, resource);
  for (int i = 0; i < 1000; ++i)
    for (int j = 0; j < 3000; ++j)
      std::copy_backward(data.begin(), data.begin() + j, l.end());
  stop_counters(time, resource);
  report_performance(__FILE__, "deque 2 list", time, resource);
  clear_counters(time, resource);

  d.assign(3000, 1);

  std::list<int>::iterator lit;
  start_counters(time, resource);
  for (int i = 0; i < 200; ++i)
    {
      lit = l.begin();
      for (int j = 0; j < 3000; ++j, ++lit)
	std::copy_backward(l.begin(), lit, d.end());
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "list 2 deque", time, resource);

  return 0;
}
