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

static_assert(__gnu_test::is_customization_point_object(std::ranges::rend));

struct R1
{
  int i = 0;
  int j = 0;

  constexpr const int* rbegin() const { return &i; }
  constexpr const int* rend() const { return &i + 1; }
  friend constexpr const int* rbegin(const R1&&); // not defined
  friend constexpr const int* rend(const R1&&); // not defined
};

// N.B. this is a lie, rend on an R1 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R1> = true;

void
test01()
{
  constexpr R1 r;

  // decay-copy(t.rend()) if it is a valid expression
  // and its type S models sentinel_for<decltype(ranges::rbegin(E))>

  static_assert( std::ranges::rend(r) == &r.i + 1 );
  static_assert( std::ranges::rend(std::move(r)) == &r.i + 1 );
}

struct R2
{
  int i = 0;

  int* rbegin() noexcept { return &i + 1; }
  long* rend() noexcept { return nullptr; } // not a sentinel for rbegin()

  friend long* rbegin(R2&) noexcept { return nullptr; }
  friend int* rend(R2& r) { return &r.i; }
};

void
test02()
{
  R2 r;

  // Otherwise, decay-copy(rend(t)) if it is a valid expression
  // and its type S models sentinel_for<decltype(ranges::rbegin(E))>

  auto i1 = std::ranges::rbegin(r);
  auto i2 = rend(r);
  static_assert( std::sentinel_for<decltype(i2), decltype(i1)> );
  VERIFY( std::ranges::rend(r) == &r.i );
  static_assert( !noexcept(std::ranges::rend(r)) );
}

struct R3
{
  int a[2] = { };
  long l[2] = { };

  constexpr const int* begin() const { return a; }
  constexpr const int* end() const { return a + 2; }

  friend constexpr const long* begin(const R3&& r) { return r.l; }
  friend constexpr const long* end(const R3&& r) { return r.l + 2; }
};

// N.B. this is a lie, begin/end on an R3 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R3> = true;

void
test03()
{
  constexpr R3 r;

  // Otherwise, make_reverse_iterator(ranges::begin(t)) if both
  // ranges::begin(t) and ranges::end(t) are valid expressions
  // of the same type I which models bidirectional_iterator.

  static_assert( std::ranges::rend(r)
      == std::make_reverse_iterator(std::ranges::begin(r)) );
  static_assert( std::ranges::rend(std::move(r))
      == std::make_reverse_iterator(std::ranges::begin(std::move(r))) );
}

void
test04()
{
  struct R4
  : __gnu_test::test_range<int, __gnu_test::bidirectional_iterator_wrapper>
  {
    R4(int (&a)[2]) : test_range(a) { }

    using test_range::begin;

    // Replace test_range::end() to return same type as begin()
    // so ranges::rend will wrap it in a reverse_iterator.
    auto end() &
    {
      using __gnu_test::bidirectional_iterator_wrapper;
      return bidirectional_iterator_wrapper<int>(bounds.last, &bounds);
    }
  };

  int a[2] = { };
  R4 r(a);
  VERIFY( std::ranges::rend(r) == std::make_reverse_iterator(std::ranges::begin(r)) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
