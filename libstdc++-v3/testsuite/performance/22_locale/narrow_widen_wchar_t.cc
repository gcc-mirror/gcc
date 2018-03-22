// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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


#include <locale>
#include <testsuite_performance.h>

int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  wchar_t bufwc[] = L"Mi innamoravo di tutto (Fabrizio De Andre')";
  char bufc[sizeof(bufwc) / sizeof(wchar_t)];

  locale loc;
  const ctype<wchar_t>& ct = use_facet<ctype<wchar_t> >(loc);

  // narrow
  start_counters(time, resource);
  for (long i = 0; i < 200000000; ++i)
    ct.narrow(i % 128, '*');
  stop_counters(time, resource);
  report_performance(__FILE__, "narrow", time, resource);
  clear_counters(time, resource);

  // narrow array
  start_counters(time, resource);
  for (long i = 0; i < 20000000; ++i)
    ct.narrow(bufwc, bufwc + sizeof(bufwc) / sizeof(wchar_t), '*', bufc);
  stop_counters(time, resource);
  report_performance(__FILE__, "narrow array", time, resource);
  clear_counters(time, resource);

  // widen
  start_counters(time, resource);
  for (long i = 0; i < 200000000; ++i)
    ct.widen(i % 128);
  stop_counters(time, resource);
  report_performance(__FILE__, "widen", time, resource);
  clear_counters(time, resource);

  // widen array
  start_counters(time, resource);
  for (long i = 0; i < 20000000; ++i)
    ct.widen(bufc, bufc + sizeof(bufc), bufwc);
  stop_counters(time, resource);
  report_performance(__FILE__, "widen array", time, resource);

  return 0;
}
