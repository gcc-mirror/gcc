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

// { dg-do compile { target c++20 } }

#include <iterator>

struct none;

template<typename T>
  concept has_inc_traits_type
    = requires { typename std::incrementable_traits<T>::difference_type; };

// Check std::incrementable_traits<T>::difference_type is U (or doesn't exist).
template<typename T, typename U>
  concept check_inc_traits = (has_inc_traits_type<T> != std::same_as<U, none>);

static_assert( check_inc_traits<void, none> );
static_assert( check_inc_traits<const void, none> );
static_assert( check_inc_traits<void*, none> );
static_assert( check_inc_traits<const void*, none> );

static_assert( check_inc_traits<int, int> );
static_assert( check_inc_traits<const int, int> );

static_assert( check_inc_traits<int*, std::ptrdiff_t> );
static_assert( check_inc_traits<const int*, std::ptrdiff_t> );
static_assert( check_inc_traits<int[2], std::ptrdiff_t> );
static_assert( check_inc_traits<const int[2], std::ptrdiff_t> );

struct A { using difference_type = int; };
static_assert( check_inc_traits<A, int> );
static_assert( check_inc_traits<const A, int> );
struct B : private A { };
static_assert( check_inc_traits<B, none> );

struct C { };
short operator-(C, C) { return 0; }
static_assert( check_inc_traits<C, short> );
static_assert( check_inc_traits<const C, short> );

struct D { };
unsigned short operator-(D, D) { return 0; }
static_assert( check_inc_traits<D, short> );
static_assert( check_inc_traits<const D, short> );

struct E { };
template<>
  struct std::incrementable_traits<E> { using difference_type = long; };
static_assert( check_inc_traits<E, long> );
static_assert( check_inc_traits<const E, long> );

template<typename T>
  concept has_alias = requires { typename std::iter_difference_t<T>; };

// Check std::iter_difference_t<T> is U (or doesn't exist).
template<typename T, typename U>
  concept check_alias = (has_alias<T> != std::same_as<U, none>);

static_assert( check_alias<void, none> );
static_assert( check_alias<const void, none> );
static_assert( check_alias<void*, none> );
static_assert( check_alias<const void*, none> );

static_assert( check_alias<int, int> );
static_assert( check_alias<const int, int> );
static_assert( check_alias<int*, std::ptrdiff_t> );
static_assert( check_alias<const int*, std::ptrdiff_t> );
static_assert( check_alias<int[2], std::ptrdiff_t> );
static_assert( check_alias<const int[2], std::ptrdiff_t> );

static_assert( check_alias<A, int> );
static_assert( check_alias<const A, int> );
static_assert( check_alias<B, none> );
static_assert( check_alias<C, short> );
static_assert( check_alias<const C, short> );
static_assert( check_alias<D, short> );
static_assert( check_alias<const D, short> );
static_assert( check_alias<E, long> );
static_assert( check_alias<const E, long> );

struct F { };
template<>
  struct std::iterator_traits<F> { using difference_type = F; };
// iterator_traits<F> is specialized, so use its difference_type.
static_assert( check_alias<F, std::iterator_traits<F>::difference_type> );

struct G { };
template<>
  struct std::incrementable_traits<G> { using difference_type = G; };
template<>
  struct std::iterator_traits<G> { using difference_type = int; };
// iterator_traits<G> is specialized, so use its difference_type.
static_assert( check_alias<G, std::iterator_traits<G>::difference_type> );

struct H { };
template<>
  struct std::incrementable_traits<H> { using difference_type = H; };
template<>
  struct std::iterator_traits<H>
  {
    using iterator_category = input_iterator_tag;
    using difference_type = int;
    using value_type = char;
    using reference = value_type&;
  };
// iterator_traits<H> is specialized, so use its difference_type.
static_assert( check_alias<H, std::iterator_traits<H>::difference_type> );

struct I
{
  using difference_type = I;
};
// iterator_traits<I> is not specialized, and no standard specialization
// matches, so use incrementable_traits.
static_assert( check_alias<I, std::incrementable_traits<I>::difference_type> );

struct J
{
  using iterator_category = std::input_iterator_tag;
  using difference_type = int;
  using value_type = char;
  using reference = value_type&;
};
// iterator_traits<J> matches constrained specialization in the library,
// so use its difference_type.
static_assert( check_alias<J, int> );
