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

#include <concepts>

static_assert( std::floating_point<float> );
static_assert( std::floating_point<double> );
static_assert( std::floating_point<long double> );
#if !defined(__STRICT_ANSI__) && defined(_GLIBCXX_USE_FLOAT128)
static_assert( std::floating_point<__float128> );
#endif

static_assert( !std::floating_point<char> );
static_assert( !std::floating_point<signed char> );
static_assert( !std::floating_point<bool> );
static_assert( !std::floating_point<int> );
static_assert( !std::floating_point<char32_t> );

#ifdef __GLIBCXX_TYPE_INT_N_0
static_assert( !std::floating_point<signed __GLIBCXX_TYPE_INT_N_0> );
#endif

static_assert( !std::floating_point<void> );
static_assert( !std::floating_point<float*> );
static_assert( !std::floating_point<float&> );
static_assert( !std::floating_point<float&&> );
static_assert( !std::floating_point<const float&> );
static_assert( !std::floating_point<float[]> );
static_assert( !std::floating_point<float[2]> );
static_assert( !std::floating_point<float()> );
static_assert( !std::floating_point<float(*)()> );
static_assert( !std::floating_point<float(&)()> );

enum E { };
static_assert( !std::floating_point<E> );
enum class CE { };
static_assert( !std::floating_point<CE> );
struct A { };
static_assert( !std::floating_point<A> );
union B { };
static_assert( !std::floating_point<B> );
