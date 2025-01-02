// { dg-do run { target c++17 } }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// Check for presence/absence of nested types.

template<typename T> using res_type = typename std::hash<T>::result_type;
template<typename T> using arg_type = typename std::hash<T>::argument_type;

template<typename Opt, typename = void>
constexpr bool has_res_type = false;
template<typename Opt>
constexpr bool has_res_type<Opt, std::void_t<res_type<Opt>>> = true;
template<typename Opt, typename = void>
constexpr bool has_arg_type = false;
template<typename Opt>
constexpr bool has_arg_type<Opt, std::void_t<arg_type<Opt>>> = true;

template<typename T>
constexpr bool has_no_types
  = ! has_res_type<std::optional<T>> && ! has_arg_type<std::optional<T>>;

#if __cplusplus >= 202002L
// Nested types result_type and argument_type are not present in C++20
static_assert( has_no_types<int> );
static_assert( has_no_types<double> );
#else
// Nested types result_type and argument_type are deprecated in C++17.
using R1 = std::hash<std::optional<int>>::result_type; // { dg-warning "deprecated" "" { target c++17_only } }
using A1 = std::hash<std::optional<int>>::argument_type; // { dg-warning "deprecated" "" { target c++17_only } }
using R2 = std::hash<std::optional<char>>::result_type; // { dg-warning "deprecated" "" { target c++17_only } }
using A2 = std::hash<std::optional<char>>::argument_type; // { dg-warning "deprecated" "" { target c++17_only } }
#endif

// Disabled specializations do not have the nested types.
static_assert( has_no_types<S> );
