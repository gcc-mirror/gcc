// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

#include <regex>
#include <testsuite_performance.h>

using namespace __gnu_test;

int main()
{
  time_counter time;
  resource_counter resource;

  start_counters(time, resource);

  // this should get compiled to just L"[abcd]"
  auto re = std::wregex(L'[' + std::wstring(300, L'a') + L"bc"
                        + std::wstring(1000, 'a') + L"d]");
  bool ok = true;
  for (int i = 0; i < 100000; ++i)
    ok = ok && (std::regex_match(L"b", re) && std::regex_match(L"d", re));

  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return ok ? 0 : 1;
}
