// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-O3" }
// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <future>
#include <chrono>
#include <iostream>
#include <testsuite_hooks.h>

const int iterations = 200;

using namespace std;

template<typename Duration>
double
print(const char* desc, Duration dur)
{
  auto ns = chrono::duration_cast<chrono::nanoseconds>(dur).count();
  double d = double(ns) / iterations;
  cout << desc << ": " << ns << "ns for " << iterations
    << " calls, avg " << d << "ns per call\n";
  return d;
}

int main()
{
  promise<int> p;
  future<int> f = p.get_future();

  auto start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_for(chrono::seconds(0));
  auto stop = chrono::high_resolution_clock::now();
  double wait_for_0 = print("wait_for(0s)", stop - start);

  start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_until(chrono::system_clock::time_point::min());
  stop = chrono::high_resolution_clock::now();
  double wait_until_sys_min __attribute__((unused))
    = print("wait_until(system_clock minimum)", stop - start);

  start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_until(chrono::steady_clock::time_point::min());
  stop = chrono::high_resolution_clock::now();
  double wait_until_steady_min __attribute__((unused))
    = print("wait_until(steady_clock minimum)", stop - start);

  start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_until(chrono::system_clock::time_point());
  stop = chrono::high_resolution_clock::now();
  double wait_until_sys_epoch __attribute__((unused))
    = print("wait_until(system_clock epoch)", stop - start);

  start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_until(chrono::steady_clock::time_point());
  stop = chrono::high_resolution_clock::now();
  double wait_until_steady_epoch __attribute__((unused))
    = print("wait_until(steady_clock epoch", stop - start);

  p.set_value(1);

  start = chrono::high_resolution_clock::now();
  for(int i = 0; i < iterations; i++)
    f.wait_for(chrono::seconds(0));
  stop = chrono::high_resolution_clock::now();
  double ready = print("wait_for when ready", stop - start);

  // Polling before ready with wait_for(0s) should be almost as fast as
  // after the result is ready.
  VERIFY( wait_for_0 < (ready * 10) );

  // polling before ready using wait_until(min) should not be terribly slow.
  VERIFY( wait_until_sys_min < (ready * 100) );
  // Converting from steady clock to system clock adds overhead before GCC 11.
  VERIFY( wait_until_steady_min < (ready * 500) );

  // The following two tests fail with GCC 11, see
  // https://gcc.gnu.org/pipermail/libstdc++/2020-November/051422.html

  // polling before ready using wait_until(epoch) should not be terribly slow.
  VERIFY( wait_until_sys_epoch < (ready * 100) );
  // Converting from steady clock to system clock adds overhead before GCC 11.
  VERIFY( wait_until_steady_epoch < (ready * 500) );
}
