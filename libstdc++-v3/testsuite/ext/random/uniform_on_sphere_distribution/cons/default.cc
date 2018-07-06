// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2014-04-15  Ulrich Drepper  <drepper@gmail.com>
//
// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// Class template uniform_on_sphere
// 26.5.1.6 Random number distribution requirements [rand.req.dist]

#include <ext/random>

void
test01()
{
  __gnu_cxx::uniform_on_sphere_distribution<2> u2;
  __gnu_cxx::uniform_on_sphere_distribution<3> u3;
  __gnu_cxx::uniform_on_sphere_distribution<4, double> u4;
  __gnu_cxx::uniform_on_sphere_distribution<5, float> u5;
}

int
main()
{
  test01();
  return 0;
}
