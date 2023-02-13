// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

template<typename T, typename... Args>
  concept can_construct_at = requires(T* p, Args&&... args)
  {
    p = std::construct_at(p, std::forward<Args>(args)...);
  };

static_assert( can_construct_at<int> );
static_assert( can_construct_at<int, int> );
static_assert( !can_construct_at<int, int, int> );

// Not required by C++20:
static_assert( noexcept(std::construct_at(std::declval<int*>(), 1)) );

void
test01()
{
  int i = -1;
  auto p = std::construct_at(&i);
  VERIFY( p == &i );
  VERIFY( i == 0 );
  p = std::construct_at(&i, 42);
  VERIFY( p == &i );
  VERIFY( i == 42 );
}

struct X {
  X(char&, void*) { }
};

static_assert( can_construct_at<X, char&, void*> );
static_assert( !can_construct_at<X> );
static_assert( !can_construct_at<X, char> );
static_assert( !can_construct_at<X, char&, const void*> );

static_assert( !noexcept(std::construct_at(std::declval<X*>(), std::declval<char&>(), std::declval<void*>())) );

int
main()
{
  test01();
}
