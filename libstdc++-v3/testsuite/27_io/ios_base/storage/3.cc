// 2000-12-19 bkoz

// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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

class derived : public std::ios_base
{
public:
  derived() {}
};

void test03()
{
  derived d;

  d.pword(0) = &d;
  d.iword(0) = 1;
}

int main(void)
{
  __gnu_test::set_memory_limits();
  test03();
  return 0;
}
