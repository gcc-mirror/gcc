// 2009-09-23  Paolo Carlini  <paolo.carlini@oracle.com>

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

// DR 396. what are characters zero and one.
void test01()
{
  bool test __attribute__((unused)) = true;

  std::bitset<4> z1("bbab", 'a', 'b');
  std::bitset<4> z1_ref(std::string("bbab"), 0, std::string::npos, 'a', 'b');
  VERIFY( z1.to_string('a', 'b') == "bbab" );
  VERIFY( z1 == z1_ref );

  std::bitset<4> z2("11a1", 'a');
  std::bitset<4> z2_ref(std::string("11a1"), 0, std::string::npos, 'a');
  VERIFY( z2.to_string('a') == "11a1" );
  VERIFY( z2 == z2_ref );

  std::bitset<8> z3("babb", 'a', 'b');
  std::bitset<8> z3_ref(std::string("babb"), 0, std::string::npos, 'a', 'b');
  VERIFY( z3.to_string('a', 'b') == "aaaababb" );
  VERIFY( z3 == z3_ref );

  std::bitset<8> z4("1a11", 'a');
  std::bitset<8> z4_ref(std::string("1a11"), 0, std::string::npos, 'a');
  VERIFY( z4.to_string('a') == "aaaa1a11" );
  VERIFY( z4 == z4_ref );

  std::bitset<2> z5("bbab", 'a', 'b');
  std::bitset<2> z5_ref(std::string("bbab"), 0, std::string::npos, 'a', 'b');
  VERIFY( z5.to_string('a', 'b') == "bb" );
  VERIFY( z5 == z5_ref );

  std::bitset<2> z6("11a1", 'a');
  std::bitset<2> z6_ref(std::string("11a1"), 0, std::string::npos, 'a');
  VERIFY( z6.to_string('a') == "11" );
  VERIFY( z6 == z6_ref );
}

int main()
{
  test01();
  return 0;
}
