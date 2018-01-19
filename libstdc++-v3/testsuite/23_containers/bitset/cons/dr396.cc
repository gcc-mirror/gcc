// 2009-09-23  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// DR 396. what are characters zero and one.
void test01()
{
  std::bitset<4> z1(std::string("bbab"), 0, std::string::npos, 'a', 'b');
  VERIFY( z1.to_string('a', 'b') == "bbab" );

  std::bitset<4> z2(std::string("11a1"), 0, std::string::npos, 'a');
  VERIFY( z2.to_string('a') == "11a1" );

  std::bitset<8> z3(std::string("babb"), 0, std::string::npos, 'a', 'b');
  VERIFY( z3.to_string('a', 'b') == "aaaababb" );

  std::bitset<8> z4(std::string("1a11"), 0, std::string::npos, 'a');
  VERIFY( z4.to_string('a') == "aaaa1a11" );

  std::bitset<2> z5(std::string("bbab"), 0, std::string::npos, 'a', 'b');
  VERIFY( z5.to_string('a', 'b') == "bb" );

  std::bitset<2> z6(std::string("11a1"), 0, std::string::npos, 'a');
  VERIFY( z6.to_string('a') == "11" );
}

int main()
{
  test01();
  return 0;
}
