// { dg-do run { target c++11 } }

// 2009-12-31  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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
  const unsigned long long num0 = 0ULL;
  std::bitset<0> bs0(num0);
  VERIFY( bs0.to_ullong() == num0 );

  const unsigned long long num1 = 215ULL;
  std::bitset<32> bs1(num1);
  VERIFY( bs1.to_ullong() == num1 );

  const unsigned long long num2 = 215ULL;
  std::bitset<64> bs2(num2);
  VERIFY( bs2.to_ullong() == num2 );

  const unsigned long long num3 = 343353215ULL;
  std::bitset<32> bs3(num3);
  VERIFY( bs3.to_ullong() == num3 );

  const unsigned long long num4 = 343353215ULL;
  std::bitset<64> bs4(num4);
  VERIFY( bs4.to_ullong() == num4 );

  const unsigned long long num5 = 13008719539498589283ULL;
  std::bitset<64> bs5(num5);
  VERIFY( bs5.to_ullong() == num5 );

  const unsigned long long num6 = 13008719539498589283ULL;
  std::bitset<128> bs6(num6);
  VERIFY( bs6.to_ullong() == num6 );
}

int main()
{
  test01();
  return 0;
}
