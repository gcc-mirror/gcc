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

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::ssize));

using std::ptrdiff_t;

void
test01()
{
  constexpr int a[10] = { };
  static_assert( std::same_as<decltype(std::ranges::ssize(a)), ptrdiff_t> );
  static_assert( std::ranges::ssize(a) == 10 );
  static_assert( noexcept(std::ranges::ssize(a)) );

  int a2[2];
  static_assert( std::same_as<decltype(std::ranges::ssize(a2)), ptrdiff_t> );
  VERIFY( std::ranges::ssize(a2) == 2);
  static_assert( noexcept(std::ranges::ssize(a2)) );
}

void
test02()
{
  int a[3] = { };
  __gnu_test::test_sized_range<int, __gnu_test::input_iterator_wrapper> ri(a);
  VERIFY( std::ranges::ssize(ri) == 3 );
  static_assert( noexcept(std::ranges::ssize(ri)) );
}

void
test04()
{
  int a[] = { 0, 1 };
  __gnu_test::test_range<int, __gnu_test::random_access_iterator_wrapper> r(a);
  VERIFY( std::ranges::ssize(r) == std::ranges::end(r) - std::ranges::begin(r) );
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
  VERIFY( std::ranges::ssize(r) == 1 );
}

void
test06()
{
  auto i = std::views::iota(1ull, 5u);
  auto s = std::ranges::size(i);
  auto ss = std::ranges::ssize(i);
  // std::ranges::range_difference_t<decltype(i)> is larger than long long,
  // but LWG 3403 says ranges::ssize(i) returns the signed version of the
  // type that ranges::size(i) returns, not the range's difference_type.
  static_assert( std::same_as<decltype(ss), std::make_signed_t<decltype(s)>> );
  VERIFY( s == 4 );
}

void
test07()
{
#ifdef __SIZEOF_INT128__
  struct R
  {
    unsigned __int128 size() const { return 4; }
  };
  R r;
  static_assert( std::same_as<decltype(std::ranges::ssize(r)), __int128> );
  VERIFY( std::ranges::ssize(r) == 4 );
#endif
}

int
main()
{
  test01();
  test02();
  test04();
  test05();
  test06();
  test07();
}
