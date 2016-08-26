// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// 20.8.4.3 time_point arithmetic [time.point.arithmetic]
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std::chrono;
  
  time_point<system_clock> t1, t2;
  t1 += seconds(1);
  VERIFY(t2.time_since_epoch() + seconds(1) == t1.time_since_epoch());
  
  t1 -= std::chrono::seconds(1);
  VERIFY(t2.time_since_epoch() == t1.time_since_epoch());
}

// 20.8.4.5 time_point non-member arithmetic [time.point.nonmember]
void
test02()
{
  bool test __attribute__((unused)) = true;
  using namespace std::chrono;
  
  time_point<system_clock> t1;
  time_point<system_clock> t2(t1 + seconds(1));
  VERIFY(t2.time_since_epoch() == t1.time_since_epoch() + seconds(1));
 
  time_point<system_clock> t3(seconds(1) + t1);
  VERIFY(t3.time_since_epoch() == t1.time_since_epoch() + seconds(1));
  
  time_point<system_clock> t4(seconds(1));
  time_point<system_clock> t5(seconds(2));
  
  time_point<system_clock> t6(t5 - seconds(1));
  VERIFY(t6.time_since_epoch() == t4.time_since_epoch());
  
  time_point<system_clock> t7(t5 - t4);
  VERIFY(t7.time_since_epoch() == t4.time_since_epoch());
}

int
main()
{
  test01();
  test02();
  return 0;
}
