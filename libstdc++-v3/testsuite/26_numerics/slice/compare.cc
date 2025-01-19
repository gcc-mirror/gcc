// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <valarray>
#include <testsuite_hooks.h>

void
test01()
{
  std::slice s1(1, 2, 3);
  VERIFY( s1 == s1 );
  VERIFY( !(s1 != s1) );
  std::slice s2(1, 2, 3);
  VERIFY( s2 == s1 );
  VERIFY( !(s2 != s1) );
  std::slice s3(3, 2, 3);
  VERIFY( s3 != s1 );
  VERIFY( !(s3 == s1) );
  std::slice s4(1, 3, 3);
  VERIFY( s4 != s1 );
  VERIFY( !(s4 == s1) );
  std::slice s5(1, 2, 4);
  VERIFY( s5 != s1 );
  VERIFY( !(s5 == s1) );
}

int
main()
{
  test01();
}
