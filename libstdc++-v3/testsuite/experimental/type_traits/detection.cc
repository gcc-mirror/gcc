// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/type_traits>

using std::declval;
using std::ptrdiff_t;
using std::experimental::is_detected;
using std::experimental::is_detected_exact;
using std::experimental::detected_or_t;
using std::experimental::is_same_v;

// Examples taken from N4502

// archetypal helper alias for a copy assignment operation:
template <class T>
using copy_assign_t = decltype(declval<T&>() = declval<T const &>());

// plausible implementation for the is_assignable type trait:
template <class T>
  using is_copy_assignable = is_detected<copy_assign_t, T>;

// plausible implementation for an augmented is_assignable type trait
// that also checks the return type:
template <class T>
  using is_canonical_copy_assignable = is_detected_exact<T&, copy_assign_t, T>;

struct A { };
struct B { B& operator=(const B&); };
struct C { void operator=(const C&); };
struct D { D& operator=(D&); };
struct E { E& operator=(E&&); };

static_assert( is_copy_assignable<A>::value,  "A is copy assignable" );
static_assert( is_copy_assignable<B>::value,  "B is copy assignable" );
static_assert( is_copy_assignable<C>::value,  "C is copy assignable" );
static_assert( !is_copy_assignable<D>::value, "D is not copy assignable" );
static_assert( !is_copy_assignable<E>::value,  "E is not copy assignable" );

static_assert( is_canonical_copy_assignable<A>::value,
               "A has canonical copy assignment" );
static_assert( is_canonical_copy_assignable<B>::value,
               "B has canonical copy assignment" );
static_assert( !is_canonical_copy_assignable<C>::value,
               "C does not have canonical copy assignment" );
static_assert( !is_canonical_copy_assignable<D>::value,
               "D does not have canonical copy assignment" );
static_assert( !is_canonical_copy_assignable<E>::value,
               "E does not have canonical copy assignment" );

// archetypal helper alias for a particular type member:
template <class T>
  using diff_t = typename T::difference_type;
// alias the type member, if it exists, otherwise alias ptrdiff_t:
template <class Ptr>
  using difference_type = detected_or_t<ptrdiff_t, diff_t, Ptr>;

struct has { using difference_type = char; };
struct has_not { };
struct inherits : has { };
struct hides : private has { };
struct reveals : private has { using has::difference_type; };

static_assert( is_same_v<difference_type<has>,      char>,      "has" );
static_assert( is_same_v<difference_type<has_not>,  ptrdiff_t>, "has not" );
static_assert( is_same_v<difference_type<inherits>, char>,      "inherits" );
static_assert( is_same_v<difference_type<hides>,    ptrdiff_t>, "hides" );
static_assert( is_same_v<difference_type<reveals>,  char>,      "reveals" );
