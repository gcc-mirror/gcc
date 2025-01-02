// { dg-do run { target c++11 } }

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

// 20.8.4 Class template time_point [time.point]

#include <chrono>
#include <testsuite_hooks.h>

// 20.8.4.6 time_point comparisons [time.point.comparisons]
void
test01()
{
  using namespace std::chrono;
  
  time_point<system_clock> t1(seconds(1));
  time_point<system_clock> t2(seconds(1));
  time_point<system_clock> t3(seconds(2));
  
  VERIFY(t1 == t2);
  VERIFY(t1 != t3);
  VERIFY(t1 < t3);
  VERIFY(t1 <= t3);
  VERIFY(t1 <= t2);
  VERIFY(t3 > t1);
  VERIFY(t3 >= t1);
  VERIFY(t2 >= t1);  
}

int
main()
{
  test01();
  return 0;
}
