// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

void
test01()
{
  bool test [[gnu::unused]] = true;

  __gnu_cxx::uniform_inside_sphere_distribution<2> u2;
  __gnu_cxx::uniform_inside_sphere_distribution<3> u3;
  __gnu_cxx::uniform_inside_sphere_distribution<4, double> u4;
  __gnu_cxx::uniform_inside_sphere_distribution<5, float> u5;
}

void
test02()
{
  __gnu_test::implicitly_default_constructible test;
  test.operator()<__gnu_cxx::uniform_inside_sphere_distribution<2>>();
  test.operator()<__gnu_cxx::uniform_inside_sphere_distribution<2>::param_type>();
}

int
main()
{
  test01();
  test02();
}
