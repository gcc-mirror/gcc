// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }
// { dg-require-cmath "" }
//
// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

// 26.5.8.3.4 Class template negative_binomial_distribution
// [rand.dist.bern.negbin]

#include <random>
#include <functional>
#include <testsuite_random.h>

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::negative_binomial_distribution<> nbd1(5, 0.3);
  auto bnbd1 = std::bind(nbd1, eng);
  testDiscreteDist(bnbd1, [](int n)
		   { return negative_binomial_pdf(n, 5, 0.3); } );

  std::negative_binomial_distribution<> nbd2(55, 0.3);
  auto bnbd2 = std::bind(nbd2, eng);
  testDiscreteDist(bnbd2, [](int n)
		   { return negative_binomial_pdf(n, 55, 0.3); } );

  std::negative_binomial_distribution<> nbd3(10, 0.75);
  auto bnbd3 = std::bind(nbd3, eng);
  testDiscreteDist(bnbd3, [](int n)
		   { return negative_binomial_pdf(n, 10, 0.75); } );
}

int main()
{
  test01();
  return 0;
}
