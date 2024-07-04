// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <utility> // as_const
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::end));

using std::same_as;

void
test01()
{
  int a[2] = {};

  // t + extent_v<T> if E is of array type T.

  static_assert(same_as<decltype(std::ranges::end(a)), decltype(a + 2)>);
  static_assert(noexcept(std::ranges::end(a)));
  VERIFY( std::ranges::end(a) == (a + 2) );
}

void
test02()
{
  using __gnu_test::test_range;
  using __gnu_test::random_access_iterator_wrapper;
  using __gnu_test::input_iterator_wrapper;
  using __gnu_test::output_iterator_wrapper;

  int a[] = { 0, 1 };

  // Otherwise, decay-copy(t.end()) if it is a valid expression
  // and its type S models sentinel_for<decltype(ranges::begin(E))>.

  test_range<int, random_access_iterator_wrapper> r(a);
  static_assert(same_as<decltype(std::ranges::end(r)), decltype(r.end())>);
  VERIFY( std::ranges::end(r) == std::ranges::next(r.begin(), 2) );

  test_range<int, input_iterator_wrapper> i(a);
  static_assert(same_as<decltype(std::ranges::end(i)), decltype(i.end())>);
  VERIFY( std::ranges::end(i) == std::ranges::next(i.begin(), 2) );

  test_range<int, output_iterator_wrapper> o(a);
  static_assert(same_as<decltype(std::ranges::end(o)), decltype(o.end())>);
  VERIFY( std::ranges::end(o) == std::ranges::next(o.begin(), 2) );
}

struct R
{
  int a[4] = { 0, 1, 2, 3 };

  const int* begin() const;
  friend int* begin(R&&) noexcept;
  friend const int* begin(const R&&) noexcept;

  // Should be ignored because it doesn't return a sentinel for int*
  const long* end() const;

  friend int* end(R& r) { return r.a + 0; }
  friend int* end(R&& r) { return r.a + 1; }
  friend const int* end(const R& r) noexcept { return r.a + 2; }
  friend const int* end(const R&& r) noexcept { return r.a + 3; }
};

struct RV // view on an R
{
  R& r;

  const int* begin() const;

  friend int* end(RV& v) noexcept { return end(v.r); }
  friend const int* end(const RV& v) { return end(std::as_const(v.r)); }
};

// Allow ranges::begin to work with RV&&
template<> constexpr bool std::ranges::enable_borrowed_range<RV> = true;

void
test03()
{
  R r;
  const R& c = r;

  // Otherwise, decay-copy(end(t)) if it is a valid expression
  // and its type S models sentinel_for<decltype(ranges::begin(E))>.

  static_assert(same_as<decltype(std::ranges::end(r)), decltype(end(r))>);
  static_assert(!noexcept(std::ranges::end(r)));
  VERIFY( std::ranges::end(r) == end(r) );

  static_assert(same_as<decltype(std::ranges::end(c)), decltype(end(c))>);
  static_assert(noexcept(std::ranges::end(c)));
  VERIFY( std::ranges::end(c) == end(c) );

  RV v{r};
  static_assert(same_as<decltype(std::ranges::end(std::move(v))),
		decltype(end(r))>);
  static_assert(noexcept(std::ranges::end(std::move(v))));
  VERIFY( std::ranges::end(std::move(v)) == end(r) );

  const RV cv{r};
  static_assert(same_as<decltype(std::ranges::end(std::move(cv))),
		decltype(end(c))>);
  static_assert(!noexcept(std::ranges::end(std::move(cv))));
  VERIFY( std::ranges::end(std::move(cv)) == end(c) );
}

struct RR
{
  short s = 0;
  long l = 0;
  int a[4] = { 0, 1, 2, 3 };

  const void* begin() const; // return type not an iterator

  friend const short* begin(RR&) noexcept;
  short* end() noexcept { return &s; }

  friend const long* begin(const RR&) noexcept;
  const long* end() const { return &l; }

  friend const int* begin(RR&&) noexcept;
  friend int* end(RR&) { throw 1; } // not valid for rvalues
  friend int* end(RR&& r) { return r.a + 1; }

  friend const int* begin(const RR&&) noexcept;
  friend const int* end(const RR&) { throw 1; } // not valid for rvalues
  friend const int* end(const RR&& r) noexcept { return r.a + 3; }
};

// N.B. this is a lie, end on an RR rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<RR> = true;

void
test04()
{
  RR r;
  const RR& c = r;
  VERIFY( std::ranges::end(r) == &r.s );
  static_assert(noexcept(std::ranges::end(r)));

  VERIFY( std::ranges::end(std::move(r)) == &r.s );
  static_assert(noexcept(std::ranges::end(std::move(r))));

  VERIFY( std::ranges::end(c) == &r.l );
  static_assert(!noexcept(std::ranges::end(c)));

  VERIFY( std::ranges::end(std::move(c)) == &r.l );
  static_assert(!noexcept(std::ranges::end(std::move(c))));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
