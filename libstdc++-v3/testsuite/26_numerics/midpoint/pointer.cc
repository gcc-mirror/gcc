// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <numeric>
#include <testsuite_hooks.h>

const int* p = nullptr;
static_assert(std::is_same_v<decltype(std::midpoint(p, p)), decltype(p)>);
// This is a GNU extension:
static_assert(noexcept(std::midpoint(p, p)));

struct test_type { };
template<typename T> decltype(std::midpoint((T*)0, (T*)0)) try_midpoint(int);
template<typename T> test_type try_midpoint(...);
template<typename T> constexpr bool no_midpoint()
{ return std::is_same_v<decltype(try_midpoint<T>()), test_type>; }

static_assert(no_midpoint<void>());
static_assert(no_midpoint<int()>());
static_assert(no_midpoint<int&>());

constexpr int ca[3] = {};
static_assert( std::midpoint(ca, ca+3) == ca+1 );

void
test01()
{
  int a[4];
  VERIFY( std::midpoint(a, a) == a );
  VERIFY( std::midpoint(a, a+1) == a );
  VERIFY( std::midpoint(a, a+2) == a+1 );
  VERIFY( std::midpoint(a, a+3) == a+1 );
  VERIFY( std::midpoint(a, a+4) == a+2 );
  VERIFY( std::midpoint(a+1, a) == a+1 );
  VERIFY( std::midpoint(a+2, a) == a+1 );
  VERIFY( std::midpoint(a+3, a) == a+2 );
  VERIFY( std::midpoint(a+4, a) == a+2 );
}

int main()
{
  test01();
}
