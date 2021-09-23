// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <type_traits>

#ifndef __cpp_lib_void_t
# error "Feature-test macro for void_t missing"
#elif __cpp_lib_void_t != 201411
# error "Feature-test macro for void_t has wrong value"
#endif

static_assert( std::is_same<std::void_t<int, long, double>, void>::value,
               "void_t is a synonym for void" );

struct X { };
X operator+(const X& x) { return x; }

template<typename T, typename U = std::void_t<>>
struct has_unary_plus
: std::false_type
{ };

template<typename T>
struct has_unary_plus<T, std::void_t<decltype(+std::declval<X>())>>
: std::true_type
{ };

static_assert( has_unary_plus<X>::value, "void_t detects valid expressions" );
