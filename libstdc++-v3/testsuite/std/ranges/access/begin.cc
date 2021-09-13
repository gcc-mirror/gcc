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

static_assert(__gnu_test::is_customization_point_object(std::ranges::begin));

using std::same_as;

void
test01()
{
  int a[2] = {};

  static_assert(same_as<decltype(std::ranges::begin(a)), decltype(a + 0)>);
  static_assert(noexcept(std::ranges::begin(a)));
  VERIFY( std::ranges::begin(a) == (a + 0) );

  constexpr long b[2] = { };
  static_assert( std::ranges::begin(b) == (b + 0) );

  struct X { };
  using A = X[]; // unbounded array
  extern A& f();
  static_assert( same_as<decltype(std::ranges::begin(f())), X*> );
}

void
test02()
{
  using __gnu_test::test_range;
  using __gnu_test::random_access_iterator_wrapper;
  using __gnu_test::input_iterator_wrapper;
  using __gnu_test::output_iterator_wrapper;

  int a[] = { 0, 1 };

  test_range<int, random_access_iterator_wrapper> r(a);
  static_assert(same_as<decltype(std::ranges::begin(r)), decltype(r.begin())>);
  VERIFY( std::ranges::begin(r) == r.begin() );

  test_range<int, input_iterator_wrapper> i(a);
  static_assert(same_as<decltype(std::ranges::begin(i)), decltype(i.begin())>);
  VERIFY( std::ranges::begin(i) == i.begin() );

  test_range<int, output_iterator_wrapper> o(a);
  static_assert(same_as<decltype(std::ranges::begin(o)), decltype(o.begin())>);
  *std::ranges::begin(o) = 99;
  VERIFY( a[0] == 99 );
}

struct R
{
  int a[4] = { 0, 1, 2, 3 };

  friend int* begin(R& r) { return r.a + 0; }
  friend int* begin(R&& r); // this overload is not defined
  friend const int* begin(const R& r) noexcept { return r.a + 2; }
  friend const int* begin(const R&& r) noexcept; // not defined
};

struct RV // view on an R
{
  R& r;

  friend int* begin(RV& rv) { return begin(rv.r); }
  friend const int* begin(const RV& rv) noexcept { return begin(rv.r); }
};

// Allow ranges::begin to work with RV&&
template<> constexpr bool std::ranges::enable_borrowed_range<RV> = true;

void
test03()
{
  R r;
  const R& c = r;

  static_assert(same_as<decltype(std::ranges::begin(r)), decltype(begin(r))>);
  static_assert(!noexcept(std::ranges::begin(r)));
  VERIFY( std::ranges::begin(r) == begin(r) );

  static_assert(same_as<decltype(std::ranges::begin(c)), decltype(begin(c))>);
  static_assert(noexcept(std::ranges::begin(c)));
  VERIFY( std::ranges::begin(c) == begin(c) );

  RV v{r};
  // enable_borrowed_range<RV> allows ranges::begin to work for rvalues,
  // but it will call v.begin() or begin(v) on an lvalue:
  static_assert(same_as<decltype(std::ranges::begin(std::move(v))),
		decltype(begin(v))>);
  static_assert(!noexcept(std::ranges::begin(std::move(v))));
  VERIFY( std::ranges::begin(std::move(v)) == begin(v) );

  const RV cv{r};
  static_assert(same_as<decltype(std::ranges::begin(std::move(cv))),
		decltype(begin(cv))>);
  static_assert(noexcept(std::ranges::begin(std::move(cv))));
  VERIFY( std::ranges::begin(std::move(cv)) == begin(cv) );
}

struct RR
{
  short s = 0;
  long l = 0;
  int a[4] = { 0, 1, 2, 3 };

  short* begin() noexcept { return &s; }
  const long* begin() const { return &l; }

  friend int* begin(RR& r) noexcept { return r.a + 0; }
  friend int* begin(RR&& r); // not defined
  friend const int* begin(const RR& r) { return r.a + 2; }
  friend const int* begin(const RR&& r) noexcept; // not defined
};

// N.B. this is a lie, begin on an RR rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<RR> = true;

void
test04()
{
  RR r;
  const RR& c = r;
  VERIFY( std::ranges::begin(r) == &r.s );
  static_assert(noexcept(std::ranges::begin(r)));

  // calls r.begin() on an lvalue, not rvalue
  VERIFY( std::ranges::begin(std::move(r)) == std::ranges::begin(r) );
  static_assert(noexcept(std::ranges::begin(std::move(r))));

  VERIFY( std::ranges::begin(c) == &r.l );
  static_assert(!noexcept(std::ranges::begin(c)));

  // calls r.begin() on a const lvalue, not rvalue
  VERIFY( std::ranges::begin(std::move(c)) == std::ranges::begin(c) );
  static_assert(!noexcept(std::ranges::begin(std::move(c))));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
