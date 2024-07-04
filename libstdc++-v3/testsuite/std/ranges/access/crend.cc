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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::crend));

struct R1
{
  int i = 0;
  int j = 0;

  constexpr const int* rbegin() const { return &i; }
  constexpr const int* rend() const { return &i + 1; }
  friend constexpr const int* rbegin(const R1&& r) { return &r.j; }
  friend constexpr const int* rend(const R1&& r) { return &r.j + 1; }
};

// N.B. this is a lie, rend on an R1 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R1> = true;

void
test01()
{
  R1 r;
  const R1& c = r;
  VERIFY( std::ranges::crend(r) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(c) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(std::move(r)) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(std::move(c)) == std::ranges::rend(c) );
}

struct R2
{
  int a[2] = { };
  long l[2] = { };

  const int* begin() const { return a; }
  const int* end() const { return a + 2; }

  friend const long* begin(const R2&& r) { return r.l; }
  friend const long* end(const R2&& r) { return r.l + 2; }
};

// N.B. this is a lie, rend on an R2 rvalue will return a dangling pointer.
template<> constexpr bool std::ranges::enable_borrowed_range<R2> = true;

void
test02()
{
  R2 r;
  const R2& c = r;
  VERIFY( std::ranges::crend(r) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(c) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(std::move(r)) == std::ranges::rend(std::move(c)) );
  VERIFY( std::ranges::crend(std::move(c)) == std::ranges::rend(std::move(c)) );
}

struct R3
{
  int i = 0;

  const int* rbegin() const noexcept { return &i + 1; }
  const long* rend() const noexcept { return nullptr; } // not a sentinel for rbegin()

  friend const long* rbegin(const R3&) noexcept { return nullptr; }
  friend const int* rend(const R3& r) { return &r.i; }
};

struct R4
{
  int i = 0;

  // These members mean that range<R4> and range<const R4> are satisfied.
  const short* begin() const { return 0; }
  const short* end() const { return 0; }

  const int* rbegin() const noexcept { return &i + 1; }
  const long* rend() const noexcept { return nullptr; } // not a sentinel for rbegin()

  friend const long* rbegin(const R4&) noexcept { return nullptr; }
  friend const int* rend(const R4& r) { return &r.i; }
};

void
test03()
{
  R3 r;
  const R3& c = r;
#if ! __cpp_lib_ranges_as_const
  VERIFY( std::ranges::crend(r) == std::ranges::rend(c) );
  static_assert( !noexcept(std::ranges::crend(r)) );
#else
  // constant_range<const R3> is not satisfied, so crend(r) is equivalent
  // to const_sentinel{rend(r)}, which is ill-formed because range<R3>
  // is not satisfied.
  static_assert( not std::ranges::range<R3> );
  static_assert( not std::ranges::range<const R3> );
#endif
  VERIFY( std::ranges::crend(c) == std::ranges::rend(c) );
  static_assert( !noexcept(std::ranges::crend(c)) );

  R4 r4;
  const R4& c4 = r4;
  auto b = std::ranges::rbegin(r4);
  auto s0 = std::ranges::rend(r4);
  static_assert( std::ranges::__access::__adl_rend<R4&> );
  auto s = std::ranges::crend(r4);
  auto s2 = std::ranges::rend(c4);
  // VERIFY( std::ranges::crend(r4) == std::ranges::rend(c4) );
}

void
test04()
{
  int a[2] = { };
  const auto& c = a;
  VERIFY( std::ranges::crend(a) == std::ranges::rend(c) );
  VERIFY( std::ranges::crend(c) == std::ranges::rend(c) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
