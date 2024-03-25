// { dg-do run { target c++11 } }

// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// 20.8.3.3 duration arithmetic [time.duration.arithmetic] (unary member ops)
void
test01()
{
  using namespace std::chrono;
  
  duration<int> d0(3);
  duration<int> d1 = -d0;
  VERIFY(d0.count() == 3);
  VERIFY(d1.count() == -3);
  
  duration<int> d2 = (+d0);
  VERIFY(d2.count() == 3);
    
  duration<int> d3(++d2);
  VERIFY(d2.count() == 4);
  VERIFY(d3.count() == 4);
    
  duration<int> d4(d3++);
  VERIFY(d3.count() == 5);
  VERIFY(d4.count() == 4);
  
  duration<int> d5(--d4);
  VERIFY(d4.count() == 3);
  VERIFY(d5.count() == 3);
  
  duration<int> d6(d5--);
  VERIFY(d5.count() == 2);
  VERIFY(d6.count() == 3);
}

// 20.8.3.3 duration arithmetic [time.duration.arithmetic] (binary member ops)
void
test02()
{
  using namespace std::chrono;
  
  duration<int> d7(3);
  duration<int> d8(9);
  d7 += d8;
  VERIFY(d7.count() == 12);
  VERIFY(d8.count() == 9);
  
  duration<int> d9(3);
  duration<int> d10(9);
  d9 -= d10;
  VERIFY(d9.count() == -6);
  VERIFY(d10.count() == 9);
  
  duration<int> d11(9);
  int i = 3;
  d11 *= i;
  VERIFY(d11.count() == 27);
    
  duration<int> d12(12);  
  d12 /= i;
  VERIFY(d12.count() == 4);
}

int
main()
{
  test01();
  test02();
  return 0;
}
