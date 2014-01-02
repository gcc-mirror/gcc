// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }
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

// 26.5.8.2.3 Class template geometric_distribution [rand.dist.bern.geom]

#include <random>
#include <functional>
#include <testsuite_random.h>

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::geometric_distribution<> gd1(0.5);
  auto bgd1 = std::bind(gd1, eng);
  testDiscreteDist(bgd1, [](int n) { return geometric_pdf(n, 0.5); } );

  std::geometric_distribution<> gd2(0.75);
  auto bgd2 = std::bind(gd2, eng);
  testDiscreteDist(bgd2, [](int n) { return geometric_pdf(n, 0.75); } );

  // libstdc++/48114
  std::geometric_distribution<> gd3(0.25);
  auto bgd3 = std::bind(gd3, eng);
  testDiscreteDist(bgd3, [](int n) { return geometric_pdf(n, 0.25); } );
}

int main()
{
  test01();
  return 0;
}
