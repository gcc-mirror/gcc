// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

template<typename T>
struct Dereferenceable { T operator*(); };

template<typename T>
  concept has_iter_reference
    = requires { typename std::iter_reference_t<T>; };

template<typename T, typename U>
  concept is_iter_reference_for
    = has_iter_reference<U> && std::same_as<T, std::iter_reference_t<U>>;

static_assert(!has_iter_reference<Dereferenceable<void>>);
static_assert(is_iter_reference_for<int, Dereferenceable<int>>);
static_assert(is_iter_reference_for<int&, Dereferenceable<int&>>);
static_assert(is_iter_reference_for<int&&, Dereferenceable<int&&>>);
static_assert(is_iter_reference_for<int&, int*>);
static_assert(is_iter_reference_for<const int&, const int*>);

template<typename T>
  concept has_iter_rvalue_reference
    = requires { typename std::iter_rvalue_reference_t<T>; };

template<typename T, typename U>
  concept is_iter_rvalue_reference_for
    = has_iter_rvalue_reference<U>
    && std::same_as<T, std::iter_rvalue_reference_t<U>>;

static_assert(!has_iter_rvalue_reference<Dereferenceable<void>>);
static_assert(is_iter_rvalue_reference_for<int, Dereferenceable<int>>);
static_assert(is_iter_rvalue_reference_for<int&&, Dereferenceable<int&>>);
static_assert(is_iter_rvalue_reference_for<int&&, Dereferenceable<int&&>>);
static_assert(is_iter_rvalue_reference_for<int&&, int*>);
static_assert(is_iter_rvalue_reference_for<const int&&, const int*>);

// These functions should be found by ADL. std::move is not applied to result.
long iter_move(Dereferenceable<short>);
long& iter_move(Dereferenceable<short&&>);
static_assert(is_iter_rvalue_reference_for<long, Dereferenceable<short>>);
static_assert(is_iter_rvalue_reference_for<long&, Dereferenceable<short&&>>);
