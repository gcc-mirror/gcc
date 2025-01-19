// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <ios>
#include <climits>
#include <testsuite_hooks.h>

// PR libstdc++/68197

struct sbuf : std::streambuf { } sb;

void
test01()
{
  std::ios ios(&sb);
  long& i1 = ios.iword(-1);
  VERIFY( i1 == 0 );
  VERIFY( ios.bad() );
  ios.clear();
  i1 = 1;
  VERIFY( ios.iword(-1) == 0 );
  VERIFY( ios.bad() );
  ios.clear();
  long& i2 = ios.iword(INT_MIN);
  VERIFY( i2 == 0 );
  VERIFY( ios.bad() );
  ios.clear();
  i2 = 2;
  VERIFY( ios.iword(INT_MIN) == 0 );
  VERIFY( ios.bad() );
  ios.clear();

  bool caught = false;
  ios.exceptions(std::ios::badbit);
  try {
    ios.iword(-1);
  } catch (const std::exception&) {
    caught = true;
  }
  VERIFY( caught );
}

void
test02()
{
  std::ios ios(&sb);
  void*& p1 = ios.pword(-1);
  VERIFY( p1 == nullptr );
  VERIFY( ios.bad() );
  ios.clear();
  p1 = &p1;
  VERIFY( ios.pword(-1) == nullptr );
  VERIFY( ios.bad() );
  ios.clear();
  void*& p2 = ios.pword(INT_MIN);
  VERIFY( p2 == nullptr );
  VERIFY( ios.bad() );
  ios.clear();
  p2 = &p2;
  VERIFY( ios.pword(INT_MIN) == nullptr );
  VERIFY( ios.bad() );
  ios.clear();

  bool caught = false;
  ios.exceptions(std::ios::badbit);
  try {
    ios.pword(-1);
  } catch (const std::exception&) {
    caught = true;
  }
  VERIFY( caught );
}

int
main()
{
  test01();
  test02();
}
