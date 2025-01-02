// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

// 26.5.8.6.1 Class template discrete_distribution [rand.dist.samp.discrete]

#include <random>
#include <functional>
#include <testsuite_random.h>

// { dg-additional-options "-DSIMULATOR_TEST" { target simulator } }

#ifdef SIMULATOR_TEST
# define ARGS 100, 1000
#else
# define ARGS
#endif

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::discrete_distribution<> dd1({ });
  auto bdd1 = std::bind(dd1, eng);
  testDiscreteDist<ARGS>(bdd1, [](int n) { return discrete_pdf(n, { }); } );

  std::discrete_distribution<> dd2({ 1.0, 3.0, 2.0});
  auto bdd2 = std::bind(dd2, eng);
  testDiscreteDist<ARGS>(bdd2, [](int n)
		   { return discrete_pdf(n, { 1.0, 3.0, 2.0}); } );

  std::discrete_distribution<> dd3({ 2.0, 2.0, 1.0, 0.0, 4.0});
  auto bdd3 = std::bind(dd3, eng);
  testDiscreteDist<ARGS>(bdd3, [](int n)
		   { return discrete_pdf(n, { 2.0, 2.0, 1.0, 0.0, 4.0}); } );
}

int main()
{
  test01();
  return 0;
}
