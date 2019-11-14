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
  VERIFY( std::ranges::data(r) == &r.j );
  static_assert( !noexcept(std::ranges::data(r)) );
  VERIFY( std::ranges::data(c) == (R*)nullptr );
  static_assert( noexcept(std::ranges::data(c)) );
}


void
test02()
{
  int a[] = { 0, 1 };
  VERIFY( std::ranges::data(a) == a + 0 );

  __gnu_test::test_range<int, __gnu_test::contiguous_iterator_wrapper> r(a);
  VERIFY( std::ranges::data(r) == std::to_address(std::ranges::begin(r)) );
}

struct R3
{
  long l = 0;

  int* data() const { return nullptr; }
  friend long* begin(R3&& r) { return &r.l; }
  friend const long* begin(const R3&& r) { return &r.l + 1; }
};

void
test03()
{
  R3 r;
  const R3& c = r;
  VERIFY( std::ranges::data(std::move(r)) == std::to_address(std::ranges::begin(std::move(r))) );
  VERIFY( std::ranges::data(std::move(c)) == std::to_address(std::ranges::begin(std::move(c))) );
}

int
main()
{
  test01();
  test02();
  test03();
}
