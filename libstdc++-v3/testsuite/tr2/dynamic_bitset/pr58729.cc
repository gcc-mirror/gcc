// { dg-options "-std=gnu++11" }

// 2013-10-15  Edward M. Smith-Rowland  <3dw4rd@verizon.net>
//
// Copyright (C) 2013 Free Software Foundation, Inc.
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

// libstdc++/58729

#include <tr2/dynamic_bitset>
#include <testsuite_hooks.h>

void
test01()
{
  std::tr2::dynamic_bitset<> pdb2{};

  pdb2.resize(10, true);
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"1111111111"});

  pdb2.resize(15);
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"000001111111111"});

  pdb2.flip();
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"111110000000000"});

  VERIFY (pdb2.size() == 15);
  VERIFY (pdb2.count() == 5);

  pdb2.resize(20, false);
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"00000111110000000000"});

  pdb2.resize(25, true);
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"1111100000111110000000000"});

  pdb2.resize(75, true);
  VERIFY (pdb2 == std::tr2::dynamic_bitset<>{"1111111111111111111111111"
					     "1111111111111111111111111"
					     "1111100000111110000000000"});

  VERIFY (pdb2.size() == 75);
  VERIFY (pdb2.count() == 60);
}

int
main()
{
  test01();
  return 0;
}
