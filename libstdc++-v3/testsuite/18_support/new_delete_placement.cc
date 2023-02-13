// 2002-07-24 Benjamin Kosnik

// Copyright (C) 2002-2023 Free Software Foundation, Inc.
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

// 18.4.1.3 - Placement forms

#include <new>
#include <testsuite_hooks.h>

// libstdc++/7286
void test01()
{
  char c = 'c';
  void* p = &c;
  void* tmp = 0;
  operator delete(p, tmp);
  operator delete[](p, tmp);
}

int main()
{
  test01();
  return 0;
}
