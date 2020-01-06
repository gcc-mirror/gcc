// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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


#include <valarray>
#include <testsuite_performance.h>

int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;

  valarray<double> va(1000000);
  
  for (int i = 0; i < 1000000; ++i)
    va[i] = i;

  size_t lengthvalues[] = { 10, 10, 10, 10, 10, 10 };
  size_t stridevalues[] = { 1, 1, 1, 1, 1, 1 };

  valarray<size_t> lengths(lengthvalues, 6);
  valarray<size_t> stride(stridevalues, 6);

  start_counters(time, resource);
  for (int j = 0; j < 1000; ++j)
    va[gslice(0, lengths, stride)];
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
