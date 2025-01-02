// { dg-do run { target c++11 } }

// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

#include <valarray>
#include <testsuite_hooks.h>

// DR 630.
void test01()
{
  using namespace std;

  valarray<int> v1;

  v1 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  VERIFY( v1.size() == 10 );
  VERIFY( v1.min() == -1 );
  VERIFY( v1.max() == -1 );

  valarray<int> v2(0, 10);

  v2 = { };
  VERIFY( v2.size() == 0 );

  valarray<int> v3(0, 10);

  v3 = { -1, -1, -1, -1, -1 };
  VERIFY( v3.size() == 5 );
  VERIFY( v3.min() == -1 );
  VERIFY( v3.max() == -1 );

  valarray<int> v4(0, 5);

  v4 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  VERIFY( v4.size() == 10 );
  VERIFY( v4.min() == -1 );
  VERIFY( v4.max() == -1 );

  valarray<int> v5(0, 10);

  v5 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  VERIFY( v5.size() == 10 );
  VERIFY( v5.min() == -1 );
  VERIFY( v5.max() == -1 );
}

int main()
{
  test01();
  return 0;
}
