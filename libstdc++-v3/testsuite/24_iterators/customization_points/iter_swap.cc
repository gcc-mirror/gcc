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

#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::iter_swap));

struct X
{
  int value;

  constexpr X(int i) : value(i) { }

  X(const X&) = default;
  X& operator=(const X&) = default;

  constexpr X& operator=(X&& x)
  {
    value = x.value;
    x.value = -1;
    return *this;
  }
};

constexpr bool
test_X(int i, int j)
{
  X x1{i}, x2{j};
  std::ranges::iter_swap(&x1, &x2);
  return x1.value == j &&  x2.value == i;
}

static_assert( test_X(1, 2) );

void
test01()
{
  VERIFY( test_X(3, 4) );
}

int
main()
{
  test01();
}
