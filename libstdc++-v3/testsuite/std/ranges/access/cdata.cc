// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::cdata));

template<typename T>
  concept has_cdata
    = requires (T&& t) { std::ranges::cdata(std::forward<T>(t)); };

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
  static_assert( has_cdata<R&> );
  static_assert( has_cdata<const R&> );
  R r;
#if ! __cpp_lib_ranges_as_const
  VERIFY( std::ranges::cdata(r) == (R*)nullptr );
  static_assert( noexcept(std::ranges::cdata(r)) );
#else
  // constant_range<const R> is not satisfied, so cdata(r) == data(r).
  VERIFY( std::ranges::cdata(r) == &r.j );
  static_assert( ! noexcept(std::ranges::cdata(r)) );
#endif
  const R& c = r;
  VERIFY( std::ranges::cdata(c) == (R*)nullptr );
  static_assert( noexcept(std::ranges::cdata(c)) );

  // not lvalues and not borrowed ranges
  static_assert( !has_cdata<R> );
  static_assert( !has_cdata<const R> );

  struct R2
  {
    // These overloads mean that range<R2> and range<const R2> are satisfied.
    int* begin();
    int* end();
    const int* begin() const;
    const int* end() const;

    int i = 0;
    int j = 0;
    int* data() { return &j; }
    const R2* data() const noexcept { return nullptr; }
  };
  static_assert( has_cdata<R2&> );
  static_assert( has_cdata<const R2&> );
  R2 r2;
  VERIFY( std::ranges::cdata(r2) == (R2*)nullptr );
  static_assert( noexcept(std::ranges::cdata(r2)) );
  const R2& c2 = r2;
  VERIFY( std::ranges::cdata(c2) == (R2*)nullptr );
  static_assert( noexcept(std::ranges::cdata(c2)) );
}

void
test02()
{
  int a[] = { 0, 1 };
  VERIFY( std::ranges::cdata(a) == a + 0 );

  static_assert( has_cdata<int(&)[2]> );
  static_assert( !has_cdata<int(&&)[2]> );
}

struct R3
{
  static inline int i = 0;
  static inline long l = 0;

  int* data() &; // this function is not defined
  friend long* begin(R3&& r); // not defined
  friend const long* begin(const R3& r) { return &r.l; }
  friend const short* begin(const R3&&); // not defined

#if __cpp_lib_ranges_as_const
  // C++23 needs these so that range<R3> is satisfied and so that
  // possibly-const-range<R3> is not the same type as R3.
  friend long* begin(R3&);
  friend long* end(R3&);
  friend const long* end(const R3& r);
#endif
};

template<> constexpr bool std::ranges::enable_borrowed_range<R3> = true;

void
test03()
{
  static_assert( has_cdata<R3&> );
  static_assert( has_cdata<R3> );  // borrowed range
  static_assert( has_cdata<const R3&> );
  static_assert( has_cdata<const R3> );  // borrowed range

  R3 r;
  const R3& c = r;
  VERIFY( std::ranges::cdata(r) == std::ranges::data(c) );
  VERIFY( std::ranges::cdata(std::move(r)) == std::ranges::data(c) );
  VERIFY( std::ranges::cdata(std::move(c)) == std::ranges::begin(c) );
}

int
main()
{
  test01();
  test02();
  test03();
}
