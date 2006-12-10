// Copyright (C) 2006 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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
