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

static_assert(__gnu_test::is_customization_point_object(std::ranges::cbegin));

using std::same_as;

void
test01()
{
  int a[2] = {};

  static_assert(same_as<decltype(std::ranges::cbegin(a)), const int*>);
  static_assert(noexcept(std::ranges::cbegin(a)));
  VERIFY( std::ranges::cbegin(a) == (a + 0) );

  constexpr long b[2] = {};
  static_assert( std::ranges::cbegin(b) == (b + 0) );
}

struct R
{
  int a[4] = { 0, 1, 2, 3 };

  friend int* begin(R& r) { return r.a + 0; }
  friend int* begin(R&&); // this function is not defined
  friend const int* begin(const R& r) noexcept { return r.a + 2; }
  friend const int* begin(const R&&); // this function is not defined

#if __cpp_lib_ranges_as_const
  friend const int* end(const R&) noexcept; // C++23 requires this.
#endif
};

struct RV // view on an R
{
  R& r;

  friend int* begin(RV&); // this function is not defined
  friend const int* begin(const RV& rv) noexcept { return begin(std::as_const(rv.r)); }

#if __cpp_lib_ranges_as_const
  friend const int* end(const RV&) noexcept; // C++23 requires this.
#endif
};

// Allow ranges::begin to work with RV&&
template<> constexpr bool std::ranges::enable_borrowed_range<RV> = true;

void
test03()
{
  R r;
  const R& c = r;
  VERIFY(std::ranges::cbegin(r) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(c) == std::ranges::begin(c));

  RV v{r};
  VERIFY(std::ranges::cbegin(std::move(v)) == std::ranges::begin(c));
  const RV cv{r};
  VERIFY(std::ranges::cbegin(std::move(cv)) == std::ranges::begin(c));
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

#if __cpp_lib_ranges_as_const
  short* end() noexcept { return &s + 1; }   // C++23 requires this.
  const long* end() const { return &l + 1; } // C++23 requires this.
#endif
};

// N.B. this is a lie, cbegin on an RR rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<RR> = true;

void
test04()
{
  RR r;
  const RR& c = r;
  VERIFY(std::ranges::cbegin(r) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(r)) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(c) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(c)) == std::ranges::begin(c));
}

int
main()
{
  test01();
  test03();
  test04();
}
