// Copyright (C) 2004-2022 Free Software Foundation, Inc.
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

// { dg-require-effective-target hosted }

#include <debug/bitset>
#include <testsuite_hooks.h>

// libstdc++/13838
void test01()
{
  using __gnu_debug::bitset;

  bitset<4>  b0, b1;
  b0.set(1);
  b0.set(3);
  b1.set(2);
  b1.set(3);
  
  b0 |= b1;

  bitset<4>  br;
  br.set(1);
  br.set(2);
  br.set(3);

  VERIFY( b0 == br );
}

int main()
{
  test01();
}
