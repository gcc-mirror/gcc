// Copyright (C) 2019-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <type_traits>

template<typename T, typename = void>
  struct has_type : std::false_type { };

template<typename T>
  struct has_type<T, std::void_t<typename T::type>> : std::true_type { };

template<typename... T>
constexpr bool
has_common_ref()
{
  return has_type<std::common_reference<T...>>::value;
}

using std::is_same_v;
using std::common_reference_t;

void test01()
{

  static_assert( !has_common_ref<>() );
  static_assert( !has_common_ref<char(*)(), int(*)()>() );
  static_assert( !has_common_ref<void*, int>() );

  static_assert( is_same_v<common_reference_t<int>, int> );
  static_assert( is_same_v<common_reference_t<int&>, int&> );
  static_assert( is_same_v<common_reference_t<void>, void> );
  static_assert( is_same_v<common_reference_t<const void>, const void> );
  static_assert( is_same_v<common_reference_t<const void, void>, void> );
  static_assert( is_same_v<common_reference_t<void(*const)(), void(*)()>, void(*)()> );
  static_assert( is_same_v<common_reference_t<int, int>, int> );
  static_assert( is_same_v<common_reference_t<int&, int>, int> );
  static_assert( is_same_v<common_reference_t<int, int&>, int> );
  static_assert( is_same_v<common_reference_t<int&&, int>, int> );
  static_assert( is_same_v<common_reference_t<int&, int&>, int&> );
  static_assert( is_same_v<common_reference_t<int&, int&&>, const int&> );
  static_assert( is_same_v<common_reference_t<int&&, int&>, const int&> );
  static_assert( is_same_v<common_reference_t<int&&, int&&>, int&&> );
  static_assert( is_same_v<common_reference_t<int&&, const int&&>, const int&&> );
  static_assert( is_same_v<common_reference_t<int&, int&, int&&>, const int&> );
  static_assert( is_same_v<common_reference_t<int&&, int&, int&>, const int&> );
  static_assert( is_same_v<common_reference_t<char&, int&>, int> );
  static_assert( is_same_v<common_reference_t<long&, int&>, long> );
}

struct A { };
struct B { };
struct C { };

template<template<typename> class AQual, template<typename> class BQual>
struct std::basic_common_reference<A, B, AQual, BQual>
{
  using type = BQual<AQual<C>>;
};

static_assert( is_same_v<common_reference_t<A, B>, C> );
static_assert( is_same_v<common_reference_t<A&, B>, C&> );
static_assert( is_same_v<common_reference_t<A&, const B>, C&> );
static_assert( is_same_v<common_reference_t<const A, B&>, const C&> );
static_assert( is_same_v<common_reference_t<const A&, B&&>, const C&> );
static_assert( is_same_v<common_reference_t<const A, B&&>, const C&&> );

struct D { };
struct E { };
struct F { };

template<> struct std::common_type<D, E> { using type = F; };

static_assert( is_same_v<common_reference_t<D, E>, F> );
static_assert( is_same_v<common_reference_t<D&, E>, F> );
static_assert( is_same_v<common_reference_t<D&, E&&>, F> );
