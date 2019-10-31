// Copyright (C) 2019 Free Software Foundation, Inc.
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

  struct Incomplete;
  using A = Incomplete[]; // unbounded array of incomplete type
  extern A& f();
  static_assert( same_as<decltype(std::ranges::begin(f())), Incomplete*> );
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
  friend int* begin(R&& r) { return r.a + 1; }
  friend const int* begin(const R& r) noexcept { return r.a + 2; }
  friend const int* begin(const R&& r) noexcept { return r.a + 3; }
};

void
test03()
{
  R r;
  const R& c = r;

  static_assert(same_as<decltype(std::ranges::begin(r)), decltype(begin(r))>);
  static_assert(!noexcept(std::ranges::begin(r)));
  VERIFY( std::ranges::begin(r) == begin(r) );

  static_assert(same_as<decltype(std::ranges::begin(std::move(r))),
		decltype(begin(std::move(r)))>);
  static_assert(!noexcept(std::ranges::begin(std::move(r))));
  VERIFY( std::ranges::begin(std::move(r)) == begin(std::move(r)) );


  static_assert(same_as<decltype(std::ranges::begin(c)), decltype(begin(c))>);
  static_assert(noexcept(std::ranges::begin(c)));
  VERIFY( std::ranges::begin(c) == begin(c) );

  static_assert(same_as<decltype(std::ranges::begin(std::move(c))),
		decltype(begin(std::move(c)))>);
  static_assert(noexcept(std::ranges::begin(std::move(c))));
  VERIFY( std::ranges::begin(std::move(c)) == begin(std::move(c)) );
}

struct RR
{
  short s = 0;
  long l = 0;
  int a[4] = { 0, 1, 2, 3 };

  short* begin() noexcept { return &s; }
  const long* begin() const { return &l; }

  friend int* begin(RR& r) { return r.a + 0; }
  friend int* begin(RR&& r) { return r.a + 1; }
  friend const int* begin(const RR& r) { return r.a + 2; }
  friend const int* begin(const RR&& r) noexcept { return r.a + 3; }
};

void
test04()
{
  RR r;
  const RR& c = r;
  VERIFY( std::ranges::begin(r) == &r.s );
  static_assert(noexcept(std::ranges::begin(r)));

  VERIFY( std::ranges::begin(std::move(r)) == r.a + 1 );
  static_assert(!noexcept(std::ranges::begin(std::move(r))));

  VERIFY( std::ranges::begin(c) == &r.l );
  static_assert(!noexcept(std::ranges::begin(c)));

  VERIFY( std::ranges::begin(std::move(c)) == r.a + 3 );
  static_assert(noexcept(std::ranges::begin(std::move(c))));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
