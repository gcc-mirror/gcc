// 2000-12-19 bkoz

// Copyright (C) 2000-2024 Free Software Foundation, Inc.
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

// 27.4.2.5 ios_base storage functions

#include <sstream>
#include <iostream>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/gcc-bugs/2000-12/msg00413.html
void test01() 
{
  using namespace std;

  ios::xalloc();
  ios::xalloc();
  ios::xalloc();
  long x4 = ios::xalloc();

  ostringstream out("the element of crime, lars von trier");
  out.pword(++x4); // should not crash
}

int main(void)
{
  __gnu_test::set_memory_limits();
  test01();
  return 0;
}
