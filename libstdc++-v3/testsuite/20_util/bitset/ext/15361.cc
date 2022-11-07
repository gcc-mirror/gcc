// Copyright (C) 2004-2022 Free Software Foundation, Inc.
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

// { dg-require-effective-target hosted }

#include <bitset>
#include <testsuite_hooks.h>

// libstdc++/15361
void test01()
{
  using namespace std;
  using std::bitset; // Work around struct ::bitset on rtems.

  bitset<256> b;
  b.set(225);
  b.set(226);

  VERIFY( b._Find_first() == 225 );
  VERIFY( b._Find_next(225) == 226 );
}

int main()
{
  test01();
  return 0;
}
