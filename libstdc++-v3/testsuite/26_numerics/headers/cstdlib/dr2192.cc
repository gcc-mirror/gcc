// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// Math-related cstdlib bits are not freestanding.
// { dg-require-effective-target hosted }

// NB: Don't include any other headers in this file.
// LWG 2192 requires <cstdlib> to declare overloads for floating point types.
#include <cstdlib>

template<typename, typename> struct is_same { enum { value = 0 }; };
template<typename T> struct is_same<T, T> { enum { value = 1 }; };

template<typename T>
  constexpr bool check(T val) {
    return is_same<decltype(std::abs(val)), T>::value;
  }

static_assert( check(1.f), "abs(1.f) returns float" );
static_assert( check(1.),  "abs(1.) returns double" );
static_assert( check(1.l), "abs(1.l) returns long double" );
