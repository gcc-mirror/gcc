// { dg-options "-std=gnu++0x" }

// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
#include <string>
#include <testsuite_hooks.h>

struct X
{
  operator const char*() { return "10101010"; }
};

void
test01()
{
  bool test __attribute__((unused)) = true;

  X x;
  std::string s(x);
  std::bitset<32> b1(static_cast<const char*>(x));
  std::bitset<32> b2(s);
  VERIFY( b1 == b2 );
}

int
main()
{
  test01();
  return 0;
}
