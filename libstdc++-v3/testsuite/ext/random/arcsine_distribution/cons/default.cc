// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2012-10-12  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#include <ext/random>
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

void
test01()
{
  __gnu_cxx::arcsine_distribution<> u;
  VERIFY( u.a() == 0.0 );
  VERIFY( u.b() == 1.0 );
  VERIFY( u.min() == 0.0 );
  VERIFY( u.max() == 1.0 );
}

void
test02()
{
  __gnu_test::implicitly_default_constructible test;
  test.operator()<__gnu_cxx::arcsine_distribution<>>();
  test.operator()<__gnu_cxx::arcsine_distribution<>::param_type>();
}

int
main()
{
  test01();
  test02();
}
