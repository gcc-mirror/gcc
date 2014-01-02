// Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

#include <debug/bitset>
#include <testsuite_hooks.h>

// libstdc++/16020
void test01()
{
  using __gnu_debug::bitset;
  bool test __attribute__((unused)) = true;

  bitset<5> b(7);
  bitset<5> c;

  bitset<5> bb(b);
  c = bb;

  VERIFY( bb == b );
  VERIFY( c == bb );
}

int main()
{
  test01();
  return 0;
}
