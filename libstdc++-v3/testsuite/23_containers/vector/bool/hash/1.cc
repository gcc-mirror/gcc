// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <vector>

void test01()
{
  std::vector<bool> b0;
  std::hash<std::vector<bool>>  h0;
  h0(b0);

  std::vector<bool> b1(10);
  std::hash<std::vector<bool>>  h1;
  h1(b1);

  std::vector<bool> b2(100);
  std::hash<std::vector<bool>>  h2;
  h2(b2);

  std::vector<bool> b3(1000);
  std::hash<std::vector<bool>>  h3;
  h3(b3);
}

int main()
{
  test01();
  return 0;
}
