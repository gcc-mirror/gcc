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

struct R1
{
  int i = 0;
  int j = 0;

  const int* rbegin() const { return &i; }
  friend const int* rbegin(const R1&& r) { return &r.j; }
};

void
test01()
{
  R1 r;
  const R1& c = r;
  VERIFY( std::ranges::crbegin(r) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(r)) == std::ranges::rbegin(std::move(c)) );
  VERIFY( std::ranges::crbegin(c) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(c)) == std::ranges::rbegin(std::move(c)) );
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

void
test02()
{
  R2 r;
  const R2& c = r;
  VERIFY( std::ranges::crbegin(r) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(r)) == std::ranges::rbegin(std::move(c)) );
  VERIFY( std::ranges::crbegin(c) == std::ranges::rbegin(c) );
  VERIFY( std::ranges::crbegin(std::move(c)) == std::ranges::rbegin(std::move(c)) );
}

int
main()
{
  test01();
  test02();
}
