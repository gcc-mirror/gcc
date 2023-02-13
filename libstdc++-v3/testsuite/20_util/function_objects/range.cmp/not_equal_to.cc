// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

// C++20 [range.cmp]

using F = std::ranges::not_equal_to;
static_assert( std::is_default_constructible_v<F> );
static_assert( std::is_copy_constructible_v<F> );
static_assert( std::is_move_constructible_v<F> );
static_assert( std::is_copy_assignable_v<F> );
static_assert( std::is_move_assignable_v<F> );

static_assert( ! std::is_invocable_v<F> );
static_assert( ! std::is_invocable_v<F, int&> );
static_assert( ! std::is_invocable_v<F, int, void> );
static_assert( ! std::is_invocable_v<F, int, void*> );
static_assert( std::is_nothrow_invocable_r_v<bool, F&, int&, int> );
static_assert( std::is_nothrow_invocable_r_v<bool, F, const long&, char> );
static_assert( std::is_nothrow_invocable_r_v<bool, const F&, short, int&> );
static_assert( std::is_nothrow_invocable_r_v<bool, const F, const char, char> );

using T = F::is_transparent; // required typedef

static_assert( ! std::ranges::not_equal_to{}(99, 99.0) );
static_assert( std::ranges::not_equal_to{}(99, 99.01) );
static_assert( std::ranges::not_equal_to{}(99, 140L) );

void
test01()
{
  F f;
  int a[2]{};
  VERIFY( ! f(&a, (void*)&a[0]) );
  VERIFY( f(&a, (void*)&a[1]) );
  VERIFY( ! f(&a + 1, (void*)(a + 2)) );
}

struct X { };
int operator==(X, X) noexcept { return 2; }
int operator!=(X, X) { return 0; }

static_assert( std::is_nothrow_invocable_r_v<bool, F&, X, X> );

void
test02()
{
  X x;
  F f;
  VERIFY( ! f(x, x) );
}

int
main()
{
  test01();
  test02();
}
