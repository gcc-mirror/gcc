// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-cmath "" }
//
// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

// 26.5.8.2.2 Class template binomial_distribution [rand.dist.bern.bin]

#include <random>
#include <functional>
#include <testsuite_random.h>

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  // libstdc++/48114
  std::binomial_distribution<> bd3(10, 0.75);
  auto bbd3 = std::bind(bd3, eng);
  testDiscreteDist(bbd3, [](int n) { return binomial_pdf(n, 10, 0.75); } );
}

int main()
{
  test01();
  return 0;
}
