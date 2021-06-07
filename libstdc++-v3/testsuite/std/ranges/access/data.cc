// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

template<typename T>
  concept has_data
    = requires (T&& t) { std::ranges::data(std::forward<T>(t)); };

void
test01()
{
  struct R
  {
    int i = 0;
    int j = 0;
    int* data() { return &j; }
    const R* data() const noexcept { return nullptr; }
  };
  static_assert( has_data<R&> );
  static_assert( has_data<const R&> );
  R r;
  const R& c = r;
  VERIFY( std::ranges::data(r) == &r.j );
  static_assert( !noexcept(std::ranges::data(r)) );
  VERIFY( std::ranges::data(c) == (R*)nullptr );
  static_assert( noexcept(std::ranges::data(c)) );

  // not lvalues and not borrowed ranges
  static_assert( !has_data<R> );
  static_assert( !has_data<const R> );
}


void
test02()
{
  int a[] = { 0, 1 };
  VERIFY( std::ranges::data(a) == a + 0 );

  __gnu_test::test_range<int, __gnu_test::contiguous_iterator_wrapper> r(a);
  VERIFY( std::ranges::data(r) == std::to_address(std::ranges::begin(r)) );

  static_assert( has_data<int(&)[2]> );
  static_assert( has_data<decltype(r)&> );
  static_assert( !has_data<int(&&)[2]> );
  static_assert( !has_data<decltype(r)&&> );
}

struct R3
{
  static inline int i;
  static inline long l;

  int* data() & { return &i; }
  friend long* begin(const R3& r) { return &l; }
  friend const short* begin(const R3&&); // not defined
};

template<> constexpr bool std::ranges::enable_borrowed_range<R3> = true;

void
test03()
{
  static_assert( has_data<R3&> );
  static_assert( has_data<R3> );  // borrowed range
  static_assert( has_data<const R3&> );
  static_assert( has_data<const R3> );  // borrowed range

  R3 r;
  const R3& c = r;
  // PR libstdc++/100824
  // ranges::data should treat the subexpression as an lvalue
  VERIFY( std::ranges::data(std::move(r)) == &R3::i );
  VERIFY( std::ranges::data(std::move(c)) == &R3::l );

  // PR libstdc++/100824 comment 3
  // Check for member data() should use decay-copy
  struct A { int*&& data(); };
  static_assert( has_data<A&> );
}

int
main()
{
  test01();
  test02();
  test03();
}
