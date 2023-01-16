// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <atomic>

#ifndef __cpp_lib_atomic_value_initialization
# error "Feature test macro for atomic value-initialization is missing"
#elif __cpp_lib_atomic_value_initialization < 201911L
# error "Feature test macro for atomic value-initialization has wrong value"
#endif

#include <testsuite_hooks.h>

struct A
{
  constexpr A() : val(42) { }
  int val;
};

constexpr std::atomic<A> a;

void
test01()
{
  VERIFY(a.load().val == 42);
  static_assert(!std::is_nothrow_default_constructible_v<std::atomic<A>>);
}

struct B
{
  constexpr B() noexcept : val(99) { }
  int val;
};

constexpr std::atomic<B> b;

void
test02()
{
  VERIFY(b.load().val == 99);
  static_assert(std::is_nothrow_default_constructible_v<std::atomic<B>>);
}

constexpr std::atomic<int*> c;

void
test03()
{
  VERIFY(c.load() == nullptr);
  static_assert(std::is_nothrow_default_constructible_v<std::atomic<int*>>);
}

int
main()
{
  test01();
  test02();
  test03();
}
