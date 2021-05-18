// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
// { dg-do compile { target c++2a } }

#include <ranges>

using std::ranges::__detail::__box;

using T = decltype([] { return 0; });
static_assert(std::is_empty_v<__box<T>>);
static_assert(std::is_nothrow_copy_constructible_v<__box<T>>);
static_assert(std::is_nothrow_move_constructible_v<__box<T>>);
static_assert(std::is_nothrow_constructible_v<__box<T>, std::in_place_t>);
static_assert(requires (__box<T> a) {
  a = a;
  a = std::move(a);
  a.operator*();
  a.operator->();
  a.has_value();
});

struct S
{
  S();
  ~S();
  S(const S&);
  S(S&&);
  S& operator=(const S&);
  S& operator=(S&&);
};
static_assert(std::is_empty_v<__box<S>>);
static_assert(!std::is_nothrow_copy_constructible_v<__box<S>>
	      && std::is_copy_constructible_v<__box<S>>);
static_assert(!std::is_nothrow_move_constructible_v<__box<S>>
	      && std::is_move_constructible_v<__box<S>>);
static_assert(!std::is_nothrow_constructible_v<__box<S>, std::in_place_t>
	      && std::is_constructible_v<__box<S>, std::in_place_t>);
static_assert(requires (__box<S> a) {
  a = a;
  a = std::move(a);
  a.operator*();
  a.operator->();
  a.has_value();
});

using U = decltype([i=0] { return 0; });
static_assert(!std::is_empty_v<__box<U>>);
static_assert(std::is_nothrow_copy_constructible_v<__box<U>>);
static_assert(std::is_nothrow_move_constructible_v<__box<U>>);
static_assert(!std::is_nothrow_constructible_v<__box<U>, std::in_place_t>);
static_assert(requires (__box<U> a) {
  a = a;
  a = std::move(a);
  a.operator*();
  a.operator->();
  a.has_value();
});

constexpr bool
test01()
{
  // Verify the default constructor value-initializes the underlying object.
  __box<int> x;
  __glibcxx_assert(*x == 0);
  return true;
}
static_assert(test01());

template<bool make_semiregular>
  struct A {
    A() requires make_semiregular;
    A(int, int);
    A(std::initializer_list<int>) = delete;
  };

void
test02()
{
  // PR libstdc++/100475
  static_assert(std::semiregular<A<true>>);
  __box<A<true>> x2(std::in_place, 0, 0);

  static_assert(!std::semiregular<A<false>>);
  __box<A<false>> x1(std::in_place, 0, 0);
}
