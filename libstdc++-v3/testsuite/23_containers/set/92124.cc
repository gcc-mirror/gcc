// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <set>
#include <testsuite_allocator.h>

struct X
{
  X(int i) noexcept(true) : _i(i) { }
  X(const X& x) noexcept(false)
  {
    if (Throw) throw 0;
    _i = x._i;
  }

  // Move constructor might throw
  X(X&& x) noexcept(false)
  {
    _i = x._i;
    x._i = -x._i;
  }

  // Tracking calls to assignment functions
  X& operator=(const X&) { throw 1; }

  X& operator=(X&& x) noexcept(false)
  {
    _i = x._i;
    x._i = -x._i;
    return *this;
  }

  bool
  operator < (const X& x) const
  { return _i < x._i; }

  int _i;
  static bool Throw;
};

bool X::Throw = false;

void
test01()
{
  using A = __gnu_test::propagating_allocator<X, false>;
  A a1(1), a2(2);
  std::set<X, std::less<X>, A> s1({ X(1) }, a1), s2({ X(2) }, a2);
  X::Throw = true;
  s1 = std::move(s2);
}

int
main()
{
  test01();
}
