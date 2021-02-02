// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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


#include <cstdio>
#include <cstring>
#include <fstream>
#include <langinfo.h>
#include <iconv.h>
#include <testsuite_performance.h>

// libstdc++/11602 (do_length)
int main(int argc, char** argv)
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const int iters = 400000;

  char cbuf[1024];
  
  memset(cbuf, 'a', 1024);

  // C++ (codecvt)
  locale loc;
  const codecvt<wchar_t, char, mbstate_t>& cvt =
    use_facet<codecvt<wchar_t, char, mbstate_t> >(loc);
  mbstate_t state;
  memset(&state, 0, sizeof(state));
  start_counters(time, resource);
  for (int i = 0; i < iters; ++i)
    cvt.length(state, cbuf, cbuf + 1024, 1024);
  stop_counters(time, resource);
  report_performance(__FILE__, "C++ (codecvt)", time, resource);

  return 0;
}
