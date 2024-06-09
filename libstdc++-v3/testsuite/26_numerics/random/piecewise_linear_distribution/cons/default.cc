// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2008-12-03  Edward M. Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// C++11
// 26.5.8.6.3 Class template piecewise_linear_distribution [rand.dist.samp.plinear]
// 26.5.1.6 random number distribution requirements [rand.req.dist]

#include <random>
#include <testsuite_hooks.h>
#include <testsuite_common_types.h>

void
test01()
{
  std::piecewise_linear_distribution<> u;
  std::vector<double> interval = u.intervals();
  std::vector<double> density = u.densities();
  VERIFY( interval.size() == 2 );
  VERIFY( interval[0] == 0.0 );
  VERIFY( interval[1] == 1.0 );
  VERIFY( density.size() == 2 );
  VERIFY( density[0] == 1.0 );
  VERIFY( density[1] == 1.0 );
}

void
test02()
{
  __gnu_test::implicitly_default_constructible test;
  test.operator()<std::piecewise_linear_distribution<>>();
  test.operator()<std::piecewise_linear_distribution<>::param_type>();
}

int
main()
{
  test01();
  test02();
}
