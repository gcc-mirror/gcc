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
// { dg-do compile { target c++2a } }

#include <iterator>

struct none;

template<typename T>
  concept has_readable_traits_type
    = requires { typename std::indirectly_readable_traits<T>::value_type; };

// Check std::indirectly_readable_traits<T>::value_type is U (or doesn't exist).
template<typename T, typename U>
  concept check_readable_traits
    = (has_readable_traits_type<T> != std::same_as<U, none>);

static_assert( check_readable_traits<void, none> );
static_assert( check_readable_traits<const void, none> );
static_assert( check_readable_traits<void*, none> );
static_assert( check_readable_traits<const void*, none> );

static_assert( check_readable_traits<int, none> );
static_assert( check_readable_traits<const int, none> );

static_assert( check_readable_traits<int*, int> );
static_assert( check_readable_traits<const int*, int> );
static_assert( check_readable_traits<int[2], int> );
static_assert( check_readable_traits<const int[2], int> );

struct A { using value_type = int; };
static_assert( check_readable_traits<A, int> );
static_assert( check_readable_traits<const A, int> );
struct B : private A { };
static_assert( check_readable_traits<B, none> );

struct C { };
short operator-(C, C) { return 0; }
static_assert( check_readable_traits<C, none> );
static_assert( check_readable_traits<const C, none> );

struct D { long operator*() const { return 1L; } };
unsigned short operator-(D, D) { return 0; }
static_assert( check_readable_traits<D, none> );
static_assert( check_readable_traits<const D, none> );

struct E { };
template<>
  struct std::indirectly_readable_traits<E> { using value_type = long; };
static_assert( check_readable_traits<E, long> );
static_assert( check_readable_traits<const E, long> );

template<typename T>
  concept has_alias = requires { typename std::iter_value_t<T>; };

// Check std::iter_value_t<T> is U (or doesn't exist).
template<typename T, typename U>
  concept check_alias = (has_alias<T> != std::same_as<U, none>);

static_assert( check_alias<void, none> );
static_assert( check_alias<const void, none> );
static_assert( check_alias<void*, none> );
static_assert( check_alias<const void*, none> );

static_assert( check_alias<int, none> );
static_assert( check_alias<const int, none> );
static_assert( check_alias<int*, std::ptrdiff_t> );
static_assert( check_alias<const int*, std::ptrdiff_t> );
static_assert( check_alias<int[2], std::ptrdiff_t> );
static_assert( check_alias<const int[2], std::ptrdiff_t> );

static_assert( check_alias<A, int> );
static_assert( check_alias<const A, int> );
static_assert( check_alias<B, none> );
static_assert( check_alias<C, none> );
static_assert( check_alias<const C, none> );
static_assert( check_alias<D, none> );
static_assert( check_alias<const D, none> );
static_assert( check_alias<E, long> );
static_assert( check_alias<const E, long> );

struct F { };
template<>
  struct std::iterator_traits<F> { using value_type = F; };
// iterator_traits<F> is specialized, so use its value_type.
static_assert( check_alias<F, std::iterator_traits<F>::value_type> );

struct G { };
template<>
  struct std::indirectly_readable_traits<G> { using value_type = G; };
template<>
  struct std::iterator_traits<G> { using value_type = int; };
// iterator_traits<G> is specialized, so use its value_type.
static_assert( check_alias<G, std::iterator_traits<G>::value_type> );

struct H { };
template<>
  struct std::indirectly_readable_traits<H> { using value_type = H; };
template<>
  struct std::iterator_traits<H>
  {
    using iterator_category = input_iterator_tag;
    using difference_type = int;
    using value_type = char;
    using reference = value_type&;
  };
// iterator_traits<H> is specialized, so use its value_type.
static_assert( check_alias<H, std::iterator_traits<H>::value_type> );

struct I
{
  using value_type = I;
};
// iterator_traits<I> is not specialized, and no standard specialization
// matches, so use indirectly_readable_traits.
static_assert( check_alias<I, std::indirectly_readable_traits<I>::value_type> );

struct J
{
  using iterator_category = std::input_iterator_tag;
  using difference_type = int;
  using value_type = char;
  using reference = value_type&;
};
// iterator_traits<J> matches constrained specialization in the library,
// so use its value_type.
static_assert( check_alias<J, int> );
