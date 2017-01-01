// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2014-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// Class template logistic_distribution
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test [[gnu::unused]] = true;

  __gnu_cxx::uniform_inside_sphere_distribution<2> u(1.5);
  VERIFY( u.radius() == 1.5 );

  __gnu_cxx::uniform_inside_sphere_distribution<3> v(3.0);
  VERIFY( v.radius() == 3.0 );
}

int
main()
{
  test01();
  return 0;
}
