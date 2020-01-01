// { dg-do run { target c++11 } }

// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

// libstdc++/50268
void test01()
{
  std::bitset<1> b1(3ULL);
  VERIFY( b1.count() == 1ULL );

  std::bitset<3> b2(30ULL);
  VERIFY( b2.count() == 2ULL );

  std::bitset<6> b3(300ULL);
  VERIFY( b3.count() == 3ULL );

  std::bitset<9> b4(3000ULL);
  VERIFY( b4.count() == 5ULL );

  std::bitset<16> b5(300000ULL);
  VERIFY( b5.count() == 7ULL );

  std::bitset<24> b6(30000000ULL);
  VERIFY( b6.count() == 9ULL );

  std::bitset<32> b7(30000000000ULL);
  VERIFY( b7.count() == 13ULL );

  std::bitset<37> b8(3000000000000ULL);
  VERIFY( b8.count() == 18ULL );

  std::bitset<40> b9(30000000000000ULL);
  VERIFY( b9.count() == 16ULL );

  std::bitset<45> b10(30000000000000ULL);
  VERIFY( b10.count() == 20ULL );

  std::bitset<64> b11(30000000000000ULL);
  VERIFY( b11.count() == 20ULL );

  std::bitset<100> b12(30000000000000ULL);
  VERIFY( b12.count() == 20ULL );

  std::bitset<200> b13(30000000000000ULL);
  VERIFY( b13.count() == 20ULL );

  std::bitset<45> b14(18446744073709551615ULL);
  VERIFY( b14.count() == 45ULL );

  std::bitset<64> b15(18446744073709551615ULL);
  VERIFY( b15.count() == 64ULL );

  std::bitset<100> b16(18446744073709551615ULL);
  VERIFY( b16.count() == 64ULL );

  std::bitset<200> b17(18446744073709551615ULL);
  VERIFY( b17.count() == 64ULL );  
}

int main()
{
  test01();
  return 0;
}
