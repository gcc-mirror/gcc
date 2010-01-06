// Copyright (C) 2009 Free Software Foundation, Inc.
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


#include <future>
#include <thread>
#include <testsuite_performance.h>

void poll(std::shared_future<void> f)
{
  while (!f.is_ready())
  { }
}

int main()
{
#ifdef TEST_T1
#define thread_type true
#endif 

  using namespace __gnu_test;
  time_counter time;
  resource_counter resource;

  const int n = 20;
  std::promise<void> p;
  std::shared_future<void> f = p.get_future();
  std::thread pollers[n];
  for (int i=0; i < n; ++i)
    pollers[i] = std::thread(poll, f);

  start_counters(time, resource);

  for (int i = 0; i < 1000000; ++i)
    (void)f.is_ready();
  p.set_value();

  for (int i=0; i < n; ++i)
    pollers[i].join();

  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
