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

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::rbegin));

struct R1
{
  int i = 0;
  int j = 0;

  constexpr const int* rbegin() const { return &i; }
  friend constexpr const int* rbegin(const R1&& r) { return &r.j; }
};

// N.B. this is a lie, rbegin on an R1 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R1> = true;

void
test01()
{
  constexpr R1 r;
  // decay-copy(t.rbegin()) if it is a valid expression
  // and its type I models input_or_output_iterator.
  static_assert( std::ranges::rbegin(r) == &r.i );
  static_assert( std::ranges::rbegin(std::move(r)) == &r.i );
}

struct R2
{
  int a[2] = { };

  constexpr const int* begin() const { return a; }
  constexpr const int* end() const { return a + 2; }

  friend constexpr const long* begin(const R2&&); // not defined
  friend constexpr const long* end(const R2&&); // not defined
};

// N.B. this is a lie, begin/end on an R2 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R2> = true;

void
test02()
{
  constexpr R2 r;
  // Otherwise, decay-copy(rbegin(t)) if it is a valid expression
  // and its type I models input_or_output_iterator [...]
  static_assert( std::ranges::rbegin(r)
      == std::make_reverse_iterator(std::ranges::end(r)) );
  static_assert( std::ranges::rbegin(std::move(r))
      == std::make_reverse_iterator(std::ranges::end(std::move(r))) );
}

void
test03()
{
  struct R3
  : __gnu_test::test_range<int, __gnu_test::bidirectional_iterator_wrapper>
  {
    R3(int (&a)[2]) : test_range(a) { }

    using test_range::begin;

    // Replace test_range::end() to return same type as begin()
    // so ranges::rbegin will wrap it in a reverse_iterator .
    auto end() &
    {
      using __gnu_test::bidirectional_iterator_wrapper;
      return bidirectional_iterator_wrapper<int>(bounds.last, &bounds);
    }
  };

  int a[2] = { };
  R3 r(a);

  // Otherwise, make_reverse_iterator(ranges::end(t)) if both ranges::begin(t)
  // and ranges::end(t) are valid expressions of the same type I which models
  // bidirectional_iterator.

  VERIFY( std::ranges::rbegin(r) == std::make_reverse_iterator(std::ranges::end(r)) );
}

int
main()
{
  test01();
  test02();
  test03();
}
