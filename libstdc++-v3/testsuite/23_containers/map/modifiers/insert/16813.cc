// Copyright (C) 2004, 2006, 2009 Free Software Foundation, Inc.
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

#include <debug/map>
#include <testsuite_hooks.h>

// libstdc++/16813
void test01()
{
  using __gnu_debug::map;
  bool test __attribute__((unused)) = true;

  map<int, float> m1, m2;

  m1[3] = 3.0f;
  m1[11] = -67.0f;

  m2.insert(m1.begin(), m1.end());

  VERIFY( m1 == m2 );
}

int main()
{
  test01();
  return 0;
}
