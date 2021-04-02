// 2007-11-23  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

// 23.3.5.2 bitset members

#include <bitset>
#include <testsuite_hooks.h>

// DR 693. std::bitset::all() missing.
void test01()
{
  std::bitset<0> z1;
  VERIFY( z1.all() );
  z1.set();
  VERIFY( z1.all() );

  std::bitset<8> z2;
  VERIFY( !z2.all() );
  z2.set();
  VERIFY( z2.all() );

  std::bitset<16> z3;
  VERIFY( !z3.all() );
  z3.set();
  VERIFY( z3.all() );

  std::bitset<32> z4;
  VERIFY( !z4.all() );
  z4.set();
  VERIFY( z4.all() );

  std::bitset<64> z5;
  VERIFY( !z5.all() );
  z5.set();
  VERIFY( z5.all() );

  std::bitset<96> z6;
  VERIFY( !z6.all() );
  z6.set();
  VERIFY( z6.all() );

  std::bitset<128> z7;
  VERIFY( !z7.all() );
  z7.set();
  VERIFY( z7.all() );

  std::bitset<192> z8;
  VERIFY( !z8.all() );
  z8.set();
  VERIFY( z8.all() );

  std::bitset<1024> z9;
  VERIFY( !z9.all() );
  z9.set();
  VERIFY( z9.all() );
}

int main()
{
  test01();
  return 0;
}
