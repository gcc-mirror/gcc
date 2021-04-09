// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <unordered_set>

#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

int moves = 0;

struct X
{
  X() = default;
  X(const X&) = default;
  X(int i) : _i(i) {}

  // Move constructor might throw
  X(X&& x) noexcept(false)
  {
    this->_i = x._i;
    x._i = -1;
    ++moves;
  }

  int _i;
};

struct XHasher
{
  std::size_t
  operator()(const X& x) const noexcept
  { return x._i; }
};

struct XEqualTo
{
  bool
  operator()(const X& lhs, const X& rhs) const noexcept
  { return lhs._i == rhs._i; }
};

void
test01()
{
  using A = __gnu_test::propagating_allocator<X, false>;
  A a1(1), a2(2);
  std::unordered_set<X, XHasher, XEqualTo, A> u1(a1), u2(a2);
  u1 = { X(0), X(1), X(2) };
  u2 = { X(3), X(4), X(5) };

  moves = 0;
  u1 = std::move(u2);

  VERIFY( moves == 3 );
  VERIFY( u1.count(X(1)) == 0 );
  VERIFY( u1.count(X(3)) == 1 );
}

int
main()
{
  test01();
}
