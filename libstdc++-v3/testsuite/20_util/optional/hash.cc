// { dg-options "-std=gnu++17" }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>

class S{}; // No hash specialization

template<class T>
auto f(int) -> decltype(std::hash<std::optional<T>>(), std::true_type());

template<class T>
auto f(...) -> decltype(std::false_type());

static_assert(!decltype(f<S>(0))::value, "");

template<typename T>
constexpr bool hashable()
{ return std::is_invocable_v<std::hash<T>&, const T&>; }

static_assert(!hashable<std::optional<S>>());
static_assert(!hashable<std::optional<const S>>());
static_assert(hashable<std::optional<int>>());
static_assert(hashable<std::optional<const int>>());

int main()
{
  int x = 42;
  std::optional<int> x2 = 42;
  VERIFY(std::hash<int>()(x) == std::hash<std::optional<int>>()(x2));

  // PR libstdc++/82262
  std::optional<const int> x3 = x2;
  VERIFY(std::hash<int>()(x) == std::hash<std::optional<const int>>()(x3));
}
