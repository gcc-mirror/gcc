// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

void test01()
{
  std::bitset<0>    b0;
  std::hash<std::bitset<0>>    h0;
  h0(b0);

  std::bitset<10>   b1;
  std::hash<std::bitset<10>>   h1;
  h1(b1);

  std::bitset<100>  b2;
  std::hash<std::bitset<100>>  h2;
  h2(b2);

  std::bitset<1000> b3;
  std::hash<std::bitset<1000>> h3;
  h3(b3);
}

int main()
{
  test01();
  return 0;
}
