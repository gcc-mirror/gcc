// 2008-09-23  Paolo Carlini  <pcarlini@suse.de>

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

// 23.3.5.2 bitset members

#include <bitset>
#include <testsuite_hooks.h>

// DR 396. what are characters zero and one.
void test01()
{
  using namespace std;

  bitset<5> b5;
  string s0 = b5.to_string<char, char_traits<char>, allocator<char> >('a', 'b');
  VERIFY( s0 == "aaaaa" );

  string s1 = b5.to_string<char, char_traits<char>, allocator<char> >('b');
  VERIFY( s1 == "bbbbb" );

  b5.set(0);
  string s2 = b5.to_string<char, char_traits<char> >('c', 'd');
  VERIFY( s2 == "ccccd" );

  string s3 = b5.to_string<char, char_traits<char> >('d');
  VERIFY( s3 == "dddd1" );

  b5.set(2);
  string s4 = b5.to_string<char>('e', 'f');
  VERIFY( s4 == "eefef" );

  string s5 = b5.to_string<char>('f');
  VERIFY( s5 == "ff1f1" );

  b5.set(4);
  string s6 = b5.to_string('g', 'h');
  VERIFY( s6 == "hghgh" );

  string s7 = b5.to_string('h');
  VERIFY( s7 == "1h1h1" );
}

int main()
{
  test01();
  return 0;
}
