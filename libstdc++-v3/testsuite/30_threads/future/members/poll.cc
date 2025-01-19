// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
// { dg-skip-if "no high resolution timer support" { hppa*-*-linux* } }

#include <future>
#include <chrono>
#include <iostream>
#include <testsuite_hooks.h>

int iterations = 200;

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

static void
calibrate()
{
  /* After set_value, wait_for is faster, so use that for the
     calibration loops to avoid zero at low clock resultions.  */
  promise<int> p = {};
  future<int> f = p.get_future();
  p.set_value(1);

  auto start = chrono::high_resolution_clock::now();
  auto stop = start;
  /* Loop until the clock advances, so that start is right after a
     time increment.  */
  do
    stop = chrono::high_resolution_clock::now();
  while (start == stop);

  /* This approximates the smallest time increment we may expect to be
     able to measure.  It doesn't have to be very precise, just a
     ballpart of the right magnitude.  */
  auto tick = stop - start;

  int i = 0;
  start = stop;
  /* Now until the clock advances again, so that stop is right
     after another time increment.  */
  do
    {
      f.wait_for(chrono::seconds(0));
      stop = chrono::high_resolution_clock::now();
      i++;
    }
  while (start == stop);

  /* Aim for some 10 ticks.  This won't be quite right if now() takes
     up a significant portion of the loop time, but we'll measure
     without that and adjust in the loop below.  */
  if (iterations < i * 10)
    iterations = i * 10;

  /* We aim for some 10 ticks for the loop that's expected to be fastest,
     but even if we don't get quite that many, we're still fine.  */
  iterations /= 2;
  do
    {
      iterations *= 2;
      start = chrono::high_resolution_clock::now();
      for(int i = 0; i < iterations; i++)
	f.wait_for(chrono::seconds(0));
      stop = chrono::high_resolution_clock::now();
    }
  while (stop - start < 5 * tick);
}

int main()
{
  /* First, calibrate the iteration count so that we don't get any of
     the actual measurement loops to complete in less than the clock
     granularity.  */
  calibrate ();

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
  VERIFY( wait_for_0 < (ready * 30) );

  // Polling before ready using wait_until(min) should not be terribly
  // slow.  We hope for no more than 100x slower, but a little over
  // 100x has been observed, and since the measurements may have a lot
  // of noise, and increasing the measurement precision through
  // additional iterations would make the test run for too long on
  // systems with very low clock precision (60Hz clocks are not
  // unheard of), we tolerate a lot of error.
  VERIFY( wait_until_sys_min < (ready * 200) );
  VERIFY( wait_until_steady_min < (ready * 200) );

  // The following two tests fail with GCC 11, see
  // https://gcc.gnu.org/pipermail/libstdc++/2020-November/051422.html
#if 0
  // Polling before ready using wait_until(epoch) should not be terribly slow.
  VERIFY( wait_until_sys_epoch < (ready * 200) );
  VERIFY( wait_until_steady_epoch < (ready * 200) );
#endif
}
