// { dg-do run { target c++11 } }

// 2009-12-29  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

#include <bitset>
#include <testsuite_hooks.h>

void test01()
{
  std::bitset<0> bs0;
  VERIFY( bs0.to_ullong() == 0 );

  std::bitset<64> bs1("11010111");
  VERIFY( bs1.to_ullong() == 215 );

  std::bitset<64> bs2("10110100100010000100000101111111"
		      "01111110011111110001110001100011");
  VERIFY( bs2.to_ullong() == 13008719539498589283ULL );
}

int main()
{
  test01();
  return 0;
}
