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
#include <cwctype>
#include <cstddef>
#include <testsuite_performance.h>

int main()
{
  using namespace std;
  using namespace __gnu_test;

  time_counter time;
  resource_counter resource;
  const wchar_t str[] =
    L"Is this the real life?\n"
    L"Is this just fantasy?\n"
    L"Caught in a landslide\n"
    L"No escape from reality\n"
    L"Open your eyes\n"
    L"Look up to the skies and see\n"
    L"I'm just a poor boy\n"
    L"I need no sympathy\n"
    L"Because I'm easy come, easy go\n"
    L"Little high, little low"
    L"Anyway the wind blows\n"
    L"Doesn't really matter to me\n"
    L"To me\n"
    L"                      -- Queen\n";
  const size_t len = sizeof(str) / sizeof(str[0]) - 1;

  locale loc;
  const ctype<wchar_t>& ct = use_facet<ctype<wchar_t> >(loc);

  // C
  wctype_t w = wctype("space");
  start_counters(time, resource);
  for (int j = 0; j < 200000; ++j)
    {
      for (size_t i = 0; i < len; ++i)
	{
	  iswctype(str[i], w);
	}
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "C", time, resource);
  clear_counters(time, resource);

  // C++
  start_counters(time, resource);
  for (int j = 0; j < 200000; ++j)
    {
      for (size_t i = 0; i < len; ++i)
	{
	  ct.is(ctype_base::space, str[i]);
	}
    }
  stop_counters(time, resource);
  report_performance(__FILE__, "C++", time, resource);

  return 0;
}
