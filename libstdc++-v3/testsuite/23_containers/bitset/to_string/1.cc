// 2004-11-17  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

void test01()
{
  using namespace std;

  bitset<5> b5;
  string s0 = b5.to_string<char, char_traits<char>, allocator<char> >();
  VERIFY( s0 == "00000" );

  // DR 434. bitset::to_string() hard to use.
  b5.set(0);
  string s1 = b5.to_string<char, char_traits<char> >();
  VERIFY( s1 == "00001" );

  b5.set(2);
  string s2 = b5.to_string<char>();
  VERIFY( s2 == "00101" );

  b5.set(4);
  string s3 = b5.to_string();
  VERIFY( s3 == "10101" );
}

int main()
{
  test01();
  return 0;
}
