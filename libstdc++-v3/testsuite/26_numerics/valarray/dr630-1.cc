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

#include <valarray>
#include <testsuite_hooks.h>

// DR 630.
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  valarray<int> v1;
  const valarray<int> v2(-1, 10000);

  v1 = v2;
  VERIFY( v1.size() == v2.size() );
  VERIFY( (v1 == v2).min() == true );

  valarray<int> v3(0, 10000);
  const valarray<int> v4;

  v3 = v4;
  VERIFY( v3.size() == v4.size() );
  VERIFY( v3.size() == 0 );

  valarray<int> v5(0, 100);
  const valarray<int> v6(-1, 10000);

  v5 = v6;
  VERIFY( v5.size() == v6.size() );
  VERIFY( (v5 == v6).min() == true );

  valarray<int> v7(0, 10000);
  const valarray<int> v8(-1, 100);

  v7 = v8;
  VERIFY( v7.size() == v8.size() );
  VERIFY( (v7 == v8).min() == true );
}

int main()
{
  test01();
  return 0;
}
