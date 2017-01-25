// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  X(X const &) = default;
  X& operator=(X const&) = delete;
};

static_assert(__is_trivial(X), "X is trivial");

int constructed = 0;
int assigned = 0;

struct Y
{
  Y() = default;
  Y(Y const &) = default;
  Y& operator=(Y const&) = default;

  Y(const X&) { ++constructed; }
  Y& operator=(const X&)& { ++assigned; return *this; }
  Y& operator=(const X&)&& = delete;
  Y& operator=(X&&) = delete;
};

static_assert(__is_trivial(Y), "Y is trivial");

void
test01()
{
  X a[100];
  Y b[100];

  std::uninitialized_copy(a, a+10, b);

  VERIFY(constructed == 0);
  VERIFY(assigned == 10);
}

int
main()
{
  test01();
}
