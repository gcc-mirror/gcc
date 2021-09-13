// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// NB: Don't include any other headers in this file.
// LWG 2192 requires <cmath> to declare overloads for integral types.
#include <cmath>

template<typename, typename> struct is_same { enum { value = 0 }; };
template<typename T> struct is_same<T, T> { enum { value = 1 }; };

template<typename T, typename U = T>
  constexpr bool check(T val) {
    return is_same<decltype(std::abs(val)), U>::value;
  }

// Unsigned arguments that promote to int are valid:
static_assert( check<short, int>(1), "abs((short)1) returns int" );
static_assert( check<unsigned short, int>(1),
              "abs((unsigned short)1) returns int" );

static_assert( check(1),        "abs(1) returns int" );
static_assert( check(1l),       "abs(1l) returns long" );
static_assert( check(1ll),      "abs(1ll) returns long long" );
