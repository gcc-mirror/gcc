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
