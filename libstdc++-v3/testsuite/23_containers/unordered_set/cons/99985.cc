// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-do compile { target c++11 } }

#include <unordered_set>
#include <testsuite_allocator.h>

template<typename Alloc, typename T = typename Alloc::value_type>
  using Set = std::unordered_set<T, std::hash<T>, std::equal_to<T>, Alloc>;

// PR libstdc++/99985 - invalid constexpr function in C++11 mode

void
test01()
{
  using A = std::allocator<int>;
  A a;
  Set<A> s;
  static_assert( noexcept( Set<A>(std::move(s)) ), "non-throwing" );
  static_assert( noexcept( Set<A>(std::move(s), a) ), "non-throwing" );
}

void
test02()
{
  using A = __gnu_test::uneq_allocator<long>;
  A a;
  Set<A> s;
  static_assert( noexcept( Set<A>(std::move(s)) ), "non-throwing" );
  static_assert( ! noexcept( Set<A>(std::move(s), a) ), "throwing" );
}
