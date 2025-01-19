// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <map>
#include <testsuite_allocator.h>

struct X
{
  X() = default;
  X(const X&)
  { if (Throw) throw 1; }

  // Move constructor might throw
  X(X&&) noexcept(false) {}

  // Tracking calls to assignment functions
  X& operator=(const X&) { throw 1; }

  X& operator=(X&&) noexcept(false) { return *this; }

  static bool Throw;
};

bool X::Throw = false;

void
test01()
{
  using A = __gnu_test::propagating_allocator<std::pair<const int, X>, false>;
  A a1(1), a2(2);
  std::map<int, X, std::less<int>, A>
    m1({ { 1, X() } }, a1),
    m2({ { 2, X() } }, a2);
  X::Throw = true;
  m1 = std::move(m2);
}

int
main()
{
  test01();
}
