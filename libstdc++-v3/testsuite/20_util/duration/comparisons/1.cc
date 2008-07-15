// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// Copyright (C) 2008 Free Software Foundation
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

// 20.8.3 Class template duration [time.duration]

#include <chrono>
#include <testsuite_hooks.h>

// 20.8.3.6 duration comparisons [time.duration.comparisons]
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std::chrono;
  
  duration<int> d0(12);
  duration<int> d1(3);
  duration<int> d2(3);
  
  VERIFY(d1 < d0);  
  VERIFY(d0 > d1);
  
  VERIFY(d0 != d1);
  VERIFY(d1 == d2);
  
  VERIFY(d1 <= d2);
  VERIFY(d1 >= d2);
  
  VERIFY(d1 <= d0);
  VERIFY(d0 >= d1);  
}

int
main()
{
  test01();
  return 0;
}
