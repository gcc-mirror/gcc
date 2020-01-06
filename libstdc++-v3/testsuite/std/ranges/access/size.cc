// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void
test01()
{
  constexpr int a[10] = { };
  static_assert( std::ranges::size(a) == 10 );
  static_assert( noexcept(std::ranges::size(a)) );

  int a2[2];
  VERIFY( std::ranges::size(a2) == 2);
  static_assert( noexcept(std::ranges::size(a2)) );

  struct Incomplete;
  using A = Incomplete[2]; // bounded array of incomplete type
  extern A& f();
  static_assert( std::same_as<decltype(std::ranges::size(f())), std::size_t> );
}

void
test02()
{
  struct R
  {
    int size() { return 1; }
    long size() const noexcept { return 2; }
  };
  R r;
  const R& c = r;
  VERIFY( std::ranges::size(r) == 1 );
  static_assert( !noexcept(std::ranges::size(r)) );
  VERIFY( std::ranges::size(c) == 2L );
  static_assert( noexcept(std::ranges::size(c)) );

  int a[3] = { };
  __gnu_test::test_sized_range<int, __gnu_test::input_iterator_wrapper> ri(a);
  VERIFY( std::ranges::size(ri) == 3 );
  static_assert( noexcept(std::ranges::size(ri)) );
}

struct R3
{
  int* size() { return nullptr; }
  friend int size(R3&) noexcept { return 1; }
  friend long size(const R3&) { return 2L; }
  friend unsigned int size(R3&&) { return 3U; }
  friend unsigned long size(const R3&&) noexcept { return 4UL; }
};

void
test03()
{
  R3 r;
  const R3& c = r;
  VERIFY( std::ranges::size(r) == 1 );
  static_assert( noexcept(std::ranges::size(r)) );
  VERIFY( std::ranges::size(std::move(r)) == 3U );
  static_assert( !noexcept(std::ranges::size(std::move(r))) );
  VERIFY( std::ranges::size(c) == 2L );
  static_assert( !noexcept(std::ranges::size(c)) );
  VERIFY( std::ranges::size(std::move(c)) == 4UL );
  static_assert( noexcept(std::ranges::size(std::move(c))) );
}

void
test04()
{
  int a[] = { 0, 1 };
  __gnu_test::test_range<int, __gnu_test::random_access_iterator_wrapper> r(a);
  VERIFY( std::ranges::size(r) == unsigned(std::ranges::end(r) - std::ranges::begin(r)) );
}

struct R5
{
  int size() const noexcept { return 0; }
  R5* begin() { return this; }
  R5* end() { return this + 1; }
};

template<>
constexpr bool std::ranges::disable_sized_range<R5> = true;

void
test05()
{
  R5 r;
  VERIFY( std::ranges::size(r) == 1 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}
