// { dg-options "-DSIMULATOR_TEST" { target simulator } }
// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-cmath "" }
//
// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

// 26.5.8.4.1 Class template poisson_distribution [rand.dist.pois.poisson]

#include <random>
#include <functional>
#include <testsuite_random.h>

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::poisson_distribution<> pd1(3.0);
  auto bpd1 = std::bind(pd1, eng);
  testDiscreteDist(bpd1, [](int n) { return poisson_pdf(n, 3.0); } );

  std::poisson_distribution<> pd2(15.0);
  auto bpd2 = std::bind(pd2, eng);
  testDiscreteDist(bpd2, [](int n) { return poisson_pdf(n, 15.0); } );

  std::poisson_distribution<> pd3(30.0);
  auto bpd3 = std::bind(pd3, eng);
  testDiscreteDist(bpd3, [](int n) { return poisson_pdf(n, 30.0); } );

  // This can take extremely long on simulators, timing out the test.
#ifndef SIMULATOR_TEST
  // libstdc++/83237
  std::poisson_distribution<> pd4(37.17);
  auto bpd4 = std::bind(pd4, eng);
  testDiscreteDist<100, 2000000>(bpd4, [](int n)
				 { return poisson_pdf(n, 37.17); } );
#endif
}

int main()
{
  test01();
  return 0;
}
