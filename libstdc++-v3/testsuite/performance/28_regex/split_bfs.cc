// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

// 2013-10-26  Tim Shen  <timshen91@gmail.com>

#include <testsuite_performance.h>
#define _GLIBCXX_REGEX_DFS_QUANTIFIERS_LIMIT 0
#include "split.h"

using namespace __gnu_test;

int main()
{
  time_counter time;
  resource_counter resource;

  source = source + source;
  source = source + source;
  source = source + source;
  source = source + source;
  source = source + source;
  source = source + source;
  source = source + source;
  source = source + source;

  start_counters(time, resource);
  split(source);
  stop_counters(time, resource);
  report_performance(__FILE__, "", time, resource);

  return 0;
}
