// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2011-2026 Free Software Foundation, Inc.
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
#include <cstdint>
#include <functional>
#include <testsuite_random.h>

// { dg-additional-options "-DSIMULATOR_TEST" { target simulator } }

#ifdef SIMULATOR_TEST
# define ARGS 100, 1000
#else
# define ARGS
#endif

template<std::uint64_t Offset>
struct shifted
{
  using result_type = std::uint64_t;
  static constexpr result_type min() { return Offset; }
  static constexpr result_type max() { return Offset + std::mt19937::max(); }

  result_type operator()()
  { return Offset + eng(); }

  std::mt19937 eng;
};

void test01()
{
  using namespace __gnu_test;

  std::mt19937 eng;

  std::uniform_int_distribution<> uid1(0, 2);
  auto buid1 = std::bind(uid1, eng);
  testDiscreteDist<ARGS>(buid1, [](int n) { return uniform_int_pdf(n, 0, 2); } );

  std::uniform_int_distribution<> uid2(3, 7);
  auto buid2 = std::bind(uid2, eng);
  testDiscreteDist<ARGS>(buid2, [](int n) { return uniform_int_pdf(n, 3, 7); } );

  std::uniform_int_distribution<> uid3(1, 20);
  auto buid3 = std::bind(uid3, eng);
  testDiscreteDist<ARGS>(buid3, [](int n) { return uniform_int_pdf(n, 1, 20); } );

  shifted<(std::uint64_t(1) << 16)> s16e;
  auto buid4 = std::bind(uid3, s16e);
  testDiscreteDist<ARGS>(buid4, [](int n) { return uniform_int_pdf(n, 1, 20); } );

  shifted<(std::uint64_t(1) << 32)> s32e;
  auto buid5 = std::bind(uid3, s32e);
  testDiscreteDist<ARGS>(buid5, [](int n) { return uniform_int_pdf(n, 1, 20); } );
}

int main()
{
  test01();
  return 0;
}
