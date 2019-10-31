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
  friend int* begin(R&& r) { return r.a + 1; }
  friend const int* begin(const R& r) noexcept { return r.a + 2; }
  friend const int* begin(const R&& r) noexcept { return r.a + 3; }
};

void
test03()
{
  R r;
  const R& c = r;
  VERIFY(std::ranges::cbegin(r) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(r)) == std::ranges::begin(std::move(c)));
  VERIFY(std::ranges::cbegin(c) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(c)) == std::ranges::begin(std::move(c)));
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
  VERIFY(std::ranges::cbegin(r) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(r)) == std::ranges::begin(std::move(c)));
  VERIFY(std::ranges::cbegin(c) == std::ranges::begin(c));
  VERIFY(std::ranges::cbegin(std::move(c)) == std::ranges::begin(std::move(c)));
}

int
main()
{
  test01();
  test03();
  test04();
}
