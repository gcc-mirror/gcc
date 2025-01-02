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

// 20.8.3 Class template duration [time.duration]

#include <chrono>
#include <testsuite_hooks.h>

// 20.8.3.5 duration non-member arithmetic [time.duration.nonmember]
void
test01()
{
  using namespace std::chrono;
  
  duration<int> d0(12);
  duration<int> d1(3);
  int i = 3;
  
  duration<int> d2 = d0 + d1;
  VERIFY(d2.count() == 15);
  
  duration<int> d3 = d0 - d1;
  VERIFY(d3.count() == 9);

  duration<int> d4 = d0 * i;
  VERIFY(d4.count() == 36);
  
  duration<int> d5 = i * d0;
  VERIFY(d5.count() == 36);
 
  duration<int> d6 = d0 / i;
  VERIFY(d6.count() == 4);
  
  int j = d0 / d1;
  VERIFY(j == 4);
}

int
main()
{
  test01();
  return 0;
}
