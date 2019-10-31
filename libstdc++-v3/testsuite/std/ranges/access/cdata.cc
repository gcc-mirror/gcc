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

void
test01()
{
  struct R
  {
    int i = 0;
    int j = 0;
    int* data() { return &j; }
    const R* data() const noexcept { return nullptr; }
  };
  R r;
  const R& c = r;
  VERIFY( std::ranges::cdata(r) == (R*)nullptr );
  static_assert( noexcept(std::ranges::cdata(r)) );
  VERIFY( std::ranges::cdata(c) == (R*)nullptr );
  static_assert( noexcept(std::ranges::cdata(c)) );
}

void
test02()
{
  int a[] = { 0, 1 };
  VERIFY( std::ranges::cdata(a) == a + 0 );
}

struct R
{
  long l = 0;

  int* data() const { return nullptr; }
  friend long* begin(R&& r) { return &r.l; }
  friend const long* begin(const R&& r) { return &r.l + 1; }
};

void
test03()
{
  R r;
  const R& c = r;
  VERIFY( std::ranges::cdata(std::move(r)) == std::ranges::data(std::move(c)) );
  VERIFY( std::ranges::cdata(std::move(c)) == std::ranges::data(std::move(c)) );
}

int
main()
{
  test01();
  test02();
  test03();
}
