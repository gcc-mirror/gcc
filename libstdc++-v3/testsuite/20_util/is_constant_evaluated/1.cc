// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <type_traits>
#include <testsuite_hooks.h>

template<int N>
struct X { int v = N; };
X<std::is_constant_evaluated()> x; // type X<true>
int y = 4;
int a = std::is_constant_evaluated() ? y : 1; // initializes a to 1
int b = std::is_constant_evaluated() ? 2 : y; // initializes b to 2
int c = y + (std::is_constant_evaluated() ? 2 : y); // initializes c to 2*y
int d = std::is_constant_evaluated(); // initializes d to 1
int e = d + std::is_constant_evaluated(); // initializes e to 1 + 0

constexpr int
foo(int x)
{
  const int n = std::is_constant_evaluated() ? 13 : 17; // n == 13
  int m = std::is_constant_evaluated() ? 13 : 17; // m might be 13 or 17 (see below)
  char arr[n] = {}; // char[13]
  return m + sizeof (arr) + x;
}

constexpr int
bar()
{
  const int n = std::is_constant_evaluated() ? 13 : 17;
  X<n> x1;
  X<std::is_constant_evaluated() ? 13 : 17> x2;
  static_assert(std::is_same<decltype(x1), decltype(x2)>::value,
		"x1/x2's type");
  return x1.v + x2.v;
}

int p = foo(0); // m == 13; initialized to 26
int q = p + foo(0); // m == 17 for this call; initialized to 56
static_assert(bar() == 26, "bar");

struct S { int a, b; };

S s = { std::is_constant_evaluated() ? 2 : 3, y };
S t = { std::is_constant_evaluated() ? 2 : 3, 4 };

static_assert(std::is_same<decltype(x), X<true> >::value, "x's type");

void
test01()
{
  VERIFY( a == 1 && b == 2 && c == 8 && d == 1 && e == 1 && p == 26 );
  VERIFY( q == 56 && s.a == 3 && s.b == 4 && t.a == 2 && t.b == 4 );
  VERIFY( foo (y) == 34 );
  if constexpr (foo (0) != 26)
    VERIFY( 0 );
  constexpr int w = foo (0);
  VERIFY( w == 26 );
}

int main()
{
  test01();
}
