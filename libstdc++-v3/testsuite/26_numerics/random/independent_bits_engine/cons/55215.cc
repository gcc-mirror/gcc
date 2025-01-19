// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
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

#include <random>
#include <testsuite_hooks.h>

int f(int x)
{
  std::seed_seq sq(&x, &x + 1);
  auto rnd = std::independent_bits_engine<std::mt19937, 9,
					  std::uint_fast32_t>(sq);
  return std::uniform_int_distribution<int>()(rnd);
}

int g(int x)
{
  std::seed_seq sq(&x, &x + 1);
  auto rnd = std::independent_bits_engine<std::mt19937, 9,
					  std::uint_fast32_t>();
  rnd.seed(sq);
  return std::uniform_int_distribution<int>()(rnd);
}

void test01()
{
  const int f1 = f(0);
  const int f2 = f(0);

  const int g1 = g(0);
  const int g2 = g(0);
  
  VERIFY( f1 == f2 );
  VERIFY( g1 == g2 );
  VERIFY( f1 == g1 );
}

int main()
{
  test01();
  return 0;
}
