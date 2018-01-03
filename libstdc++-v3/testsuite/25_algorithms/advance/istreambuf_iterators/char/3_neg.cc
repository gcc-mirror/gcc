// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

// { dg-do run { xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <sstream>
#include <iterator>
#include <algorithm>

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> in_iterator_type;

  const char data1[] = "Drei Phantasien nach Friedrich Holderlin";
  const string str1(data1);
  istringstream iss1(str1);
  in_iterator_type beg1(iss1);

  advance(beg1, 50); // Invalid
}

int main()
{
  test01();
  return 0;
}
