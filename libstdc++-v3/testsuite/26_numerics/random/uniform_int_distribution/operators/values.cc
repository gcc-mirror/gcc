// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

// 26.5.8.2.1 Class template uniform_int_distribution [rand.dist.uni.int]

#include <random>
#include <functional>
#include <testsuite_random.h>

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::uniform_int_distribution<> uid1(0, 2);
  auto buid1 = std::bind(uid1, eng);
  testDiscreteDist(buid1, [](int n) { return uniform_int_pdf(n, 0, 2); } );

  std::uniform_int_distribution<> uid2(3, 7);
  auto buid2 = std::bind(uid2, eng);
  testDiscreteDist(buid2, [](int n) { return uniform_int_pdf(n, 3, 7); } );

  std::uniform_int_distribution<> uid3(1, 20);
  auto buid3 = std::bind(uid3, eng);
  testDiscreteDist(buid3, [](int n) { return uniform_int_pdf(n, 1, 20); } );
}

int main()
{
  test01();
  return 0;
}
