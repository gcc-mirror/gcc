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

// 26.5.8.2.1 Class template bernoulli_distribution [rand.dist.bern.bernoulli]

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

  std::bernoulli_distribution bd1(0.25);
  auto bbd1 = std::bind(bd1, eng);
  testDiscreteDist<ARGS>(bbd1, [](int n) { return bernoulli_pdf(n, 0.25); } );

  std::bernoulli_distribution bd2(0.5);
  auto bbd2 = std::bind(bd2, eng);
  testDiscreteDist<ARGS>(bbd2, [](int n) { return bernoulli_pdf(n, 0.5); } );

  std::bernoulli_distribution bd3(0.75);
  auto bbd3 = std::bind(bd3, eng);
  testDiscreteDist<ARGS>(bbd3, [](int n) { return bernoulli_pdf(n, 0.75); } );
}

int main()
{
  test01();
  return 0;
}
