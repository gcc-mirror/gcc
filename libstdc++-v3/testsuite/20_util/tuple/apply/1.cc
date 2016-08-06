// Copyright (C) 2014-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++17" }

#include <tuple>
#include <testsuite_hooks.h>

#if  __cpp_lib_apply < 201603
#  error "__cpp_lib_apply < 201603"
#endif

void
test01()
{
  auto t = std::make_tuple(1, '2', 3.0);
  std::apply( [&](int& i, char& c, double& d) {
      VERIFY(&i == &std::get<int>(t));
      VERIFY(&c == &std::get<char>(t));
      VERIFY(&d == &std::get<double>(t));
    }, t);
}

constexpr int func(int i, int j) { return i + j; }

void
test02()
{
  constexpr auto t = std::make_tuple(1, 2);
  constexpr int i = std::apply(func, t);
  VERIFY( i == 3 );
}

struct F
{
  int f(int i, int j) const { return i + j; }
};

void
test03()
{
  auto t = std::make_tuple(F{}, 1, 2);
  int r = std::apply(&F::f, t);
  VERIFY( r == 3 );
}

int
main()
{
  test01();
  test02();
  test03();
}
