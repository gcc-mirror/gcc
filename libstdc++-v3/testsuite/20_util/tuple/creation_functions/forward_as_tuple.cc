// { dg-do run { target c++11 } }

// 2010-04-30  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>
#include <type_traits>
#include <testsuite_hooks.h>

void
test01()
{
  std::forward_as_tuple();

  VERIFY( std::get<0>(std::forward_as_tuple(-1)) == -1 );
  VERIFY( (std::is_same<decltype(std::forward_as_tuple(-1)),
	   std::tuple<int&&>>::value) );

  const int i1 = 1;
  const int i2 = 2;
  const double d1 = 4.0;
  auto t1 = std::forward_as_tuple(i1, i2, d1);
  VERIFY( (std::is_same<decltype(t1), std::tuple<const int&,
	   const int&, const double&>>::value) );
  VERIFY( std::get<0>(t1) == i1 );
  VERIFY( std::get<1>(t1) == i2 );
  VERIFY( std::get<2>(t1) == d1 );

  typedef const int a_type1[3];
  a_type1 a1 = { -1, 1, 2 };
  auto t2 = std::forward_as_tuple(a1);
  VERIFY( (std::is_same<decltype(t2), std::tuple<a_type1&>>::value) );
  VERIFY( std::get<0>(t2)[0] == a1[0] );
  VERIFY( std::get<0>(t2)[1] == a1[1] );
  VERIFY( std::get<0>(t2)[2] == a1[2] );

  typedef int a_type2[2];
  a_type2 a2 = { 2, -2 };
  volatile int i4 = 1;
  auto t3 = std::forward_as_tuple(a2, i4);
  VERIFY( (std::is_same<decltype(t3), std::tuple<a_type2&,
	   volatile int&>>::value) );
  VERIFY( std::get<0>(t3)[0] == a2[0] );
  VERIFY( std::get<0>(t3)[1] == a2[1] );
  VERIFY( std::get<1>(t3) == i4 );
}

int main()
{
  test01();
  return 0;
}
