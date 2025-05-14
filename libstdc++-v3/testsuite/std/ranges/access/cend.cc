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
#include <utility> // as_const
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::cend));

using std::same_as;

void
test01()
{
  int a[2] = {};

  static_assert(same_as<decltype(std::ranges::cend(a)), const int*>);
  static_assert(noexcept(std::ranges::cend(a)));
  VERIFY( std::ranges::cend(a) == (a + 2) );
}

struct R
{
  int a[4] = { 0, 1, 2, 3 };

  const int* begin() const { return nullptr; }
  friend const int* begin(const R&&) noexcept { return nullptr; }

  // Should be ignored because it doesn't return a sentinel for int*
  const long* end() const { return nullptr; }

  friend int* end(R& r) { return r.a + 0; }
  friend int* end(R&& r) { return r.a + 1; }
  friend const int* end(const R& r) noexcept { return r.a + 2; }
  friend const int* end(const R&& r) noexcept { return r.a + 3; }
};

struct RV // view on an R
{
  R& r;

  friend const int* begin(RV& rv) { return rv.r.begin(); }
  friend int* end(RV& rv) { return end(rv.r); }
  friend const int* begin(const RV& rv) noexcept { return rv.r.begin(); }
  friend const int* end(const RV& rv) noexcept { return end(std::as_const(rv.r)); }
};

// Allow ranges::end to work with RV&&
template<> constexpr bool std::ranges::enable_borrowed_range<RV> = true;

void
test03()
{
  R r;
  const R& c = r;
  VERIFY( std::ranges::cend(r) == std::ranges::end(c) );
  VERIFY( std::ranges::cend(c) == std::ranges::end(c) );

  RV v{r};
  VERIFY( std::ranges::cend(std::move(v)) == std::ranges::end(c) );

  const RV cv{r};
  VERIFY( std::ranges::cend(std::move(cv)) == std::ranges::end(c) );
}

struct RR
{
  short s = 0;
  short l = 0;
  int a[4] = { 0, 1, 2, 3 };

  const void* begin() const; // return type not an iterator

  friend int* end(RR&) { throw 1; }
  short* end() noexcept { return &s; }

  friend const short* begin(const RR&) noexcept;
  const short* end() const { return &l; }

  friend int* begin(RR&&) noexcept;
  friend int* end(RR&& r) { return r.a + 1; }

  friend const int* begin(const RR&&) noexcept;
  friend const int* end(const RR&& r) noexcept { return r.a + 3; }
};

// N.B. this is a lie, begin/end on an RR rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<RR> = true;

void
test04()
{
  RR r;
  const RR& c = r;
  VERIFY( std::ranges::cend(r) == std::ranges::end(c) );
  VERIFY( std::ranges::cend(c) == std::ranges::end(c) );

  VERIFY( std::ranges::cend(std::move(r)) == std::ranges::end(c) );
  VERIFY( std::ranges::cend(std::move(c)) == std::ranges::end(c) );
}

int
main()
{
  test01();
  test03();
  test04();
}
