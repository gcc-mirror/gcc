// 2008-05-21  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <bitset>
#include <testsuite_hooks.h>

// DR 778. std::bitset does not have any constructor taking a string literal.
void test01()
{
  bool test __attribute__((unused)) = true;

  std::bitset<4> z1("1101");
  std::bitset<4> z1_ref(std::string("1101"));
  VERIFY( z1.to_string() == "1101" );
  VERIFY( z1 == z1_ref );

  std::bitset<8> z2("1011");
  std::bitset<8> z2_ref(std::string("1011"));
  VERIFY( z2.to_string() == "00001011" );
  VERIFY( z2 == z2_ref );

  std::bitset<2> z3("1101");
  std::bitset<2> z3_ref(std::string("1101"));
  VERIFY( z3.to_string() == "11" );
  VERIFY( z3 == z3_ref );
}

int main()
{
  test01();
  return 0;
}
