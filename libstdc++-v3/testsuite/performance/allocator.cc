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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 * 
 *
 * The goal with this application is to compare the performance
 * between different STL allocators relative to the default
 * __pool_alloc.
 *
 * The container used for the tests is vector, which as stated by
 * SGI "Vector is the simplest of the STL container classes, and in 
 * many cases the most efficient.".
 *
 * NOTE! The vector<> container does some "caching" of it's own and
 * therefore we redeclare the variable in each iteration (forcing the
 * const/destr to be called and thus free memory).
 *
 * NOTE! The usage of gettimeofday is unwanted since it's not POSIX,
 * however I have not found a more generic system call to use - 
 * ideas are greatly appriciated!
 *
 * NOTE! This version only does operations on vector<int>. More/other
 * data types should maybe also be tested - ideas are always welcome!
 *
 * I assume that glibc/underlying malloc() implementation has been
 * compiled with -O2 thus should this application also be compiled
 * with -O2 in order to get relevant results.
 */

// 2003-02-05 Stefan Olsson <stefan@snon.net>

#include <vector>
#include <sys/time.h>
#include <ext/mt_allocator.h>
#include <ext/malloc_allocator.h>
#include <testsuite_performance.h>

using namespace std;
using __gnu_cxx::malloc_allocator;
using __gnu_cxx::__mt_alloc;

/*
 * In order to avoid that the time it takes for the application to 
 * startup/shutdown affect the end result, we define a target 
 * duration (in seconds) for which all tests should run.
 * Each test is responsible for "calibrating" their # of iterations 
 * to come as close as possible to this target based on the time
 * it takes to complete the test using the default __pool_alloc.
 */
int target_run_time = 10;

/*
 * The number of iterations to be performed in order to figure out
 * the "speed" of this computer and adjust the number of iterations
 * needed to come close to target_run_time.
 */
int calibrate_iterations = 100000;

/*
 * The number of values to insert in the vector, 32 will cause 
 * 5 (re)allocations to be performed (sizes 4, 8, 16, 32 and 64)
 * This means that all allocations are within _MAX_BYTES = 128
 * as defined in stl_alloc.h for __pool_alloc.
 * Whether or not this value is relevant in "the real world" 
 * or not I don't know and should probably be investigated in 
 * more detail.
 */
int insert_values = 32;

static struct timeval _tstart, _tend;
static struct timezone tz;

void
tstart(void)
{
  gettimeofday(&_tstart, &tz);
}

void
tend(void)
{
  gettimeofday(&_tend, &tz);
}

double
tval()
{
  double t1, t2;

  t1 =(double)_tstart.tv_sec +(double)_tstart.tv_usec/(1000*1000);
  t2 =(double)_tend.tv_sec +(double)_tend.tv_usec/(1000*1000);
  return t2 - t1;
}

int
calibrate_test_ints(void)
{
  tstart();
  for (int i = 0; i < calibrate_iterations; i++)
  {
    vector<int> v1;

    for(int j = 0; j < insert_values; j++)
      v1.push_back(1);
  }
  tend();

  return(int)((double)target_run_time / tval()) * calibrate_iterations;
}

double
test_ints_pool_alloc(int iterations)
{
  tstart();
  for(int i = 0; i < iterations; i++)
  {
    vector<int> v1;

    for(int j = 0; j < insert_values; j++)
      v1.push_back(1);
  }
  tend();

  return tval();
}

double
test_ints_malloc_alloc(int iterations)
{
  tstart();
  for(int i = 0; i < iterations; i++)
  {
    vector<int, malloc_allocator<int> > v1;

    for(int j = 0; j < insert_values; j++)
    {
      v1.push_back(1);
    }
  }
  tend();

  return tval();
}

double
test_ints_mt_alloc(int iterations)
{
  tstart();
  for(int i = 0; i < iterations; i++)
  {
    vector<int, __mt_alloc<int> > v1;

    for(int j = 0; j < insert_values; j++)
    {
      v1.push_back(1);
    }
  }
  tend();

  return tval();
}

// http://gcc.gnu.org/ml/libstdc++/2001-05/msg00105.html
// http://gcc.gnu.org/ml/libstdc++/2003-05/msg00231.html
int main(void)
{
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  int iterations = calibrate_test_ints();

  start_counters(time, resource);
  test_ints_pool_alloc(iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "default", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  test_ints_malloc_alloc(iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "malloc", time, resource);
  clear_counters(time, resource);

  start_counters(time, resource);
  test_ints_mt_alloc(iterations);
  stop_counters(time, resource);
  report_performance(__FILE__, "mt", time, resource);

  return 0;
}
