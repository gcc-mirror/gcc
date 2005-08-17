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
