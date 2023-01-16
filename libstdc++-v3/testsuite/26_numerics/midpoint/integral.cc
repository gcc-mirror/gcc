// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
// { dg-do run { target c++2a } }

#include <numeric>

#ifndef __cpp_lib_interpolate
# error "Feature-test macro for midpoint and lerp missing in <numeric>"
#elif __cpp_lib_interpolate != 201902L
# error "Feature-test macro for midpoint and lerp has wrong value in <numeric>"
#endif

#include <climits>
#include <testsuite_hooks.h>

static_assert(std::is_same_v<decltype(std::midpoint(0, 1)), int>);
static_assert(noexcept(std::midpoint(1, 2)));

struct test_type { };
template<typename T> decltype(std::midpoint<T>(T(), T())) try_midpoint(int);
template<typename T> test_type try_midpoint(...);
template<typename T> constexpr bool no_midpoint()
{ return std::is_same_v<decltype(try_midpoint<T>()), test_type>; }

static_assert(no_midpoint<bool>());
static_assert(no_midpoint<const bool>());
static_assert(no_midpoint<const int>());
static_assert(no_midpoint<volatile int>());

static_assert( std::midpoint(0, 0) == 0 );
static_assert( std::midpoint(1, 1) == 1 );
static_assert( std::midpoint(0, 1) == 0 );
static_assert( std::midpoint(1, 0) == 1 );
static_assert( std::midpoint(0, 2) == 1 );
static_assert( std::midpoint(3, 2) == 3 );
static_assert( std::midpoint(-5, 4) == -1 );
static_assert( std::midpoint(5, -4) == 1 );
static_assert( std::midpoint(-5, -4) == -5 );
static_assert( std::midpoint(-4, -5) == -4 );
static_assert( std::midpoint(INT_MIN, INT_MAX) == -1 );
static_assert( std::midpoint(INT_MAX, INT_MIN) == 0 );
static_assert( std::midpoint(INT_MAX, INT_MAX) == INT_MAX );
static_assert( std::midpoint(INT_MAX, INT_MAX-1) == INT_MAX );
static_assert( std::midpoint(INT_MAX-1, INT_MAX-1) == INT_MAX-1 );
static_assert( std::midpoint(INT_MAX-1, INT_MAX) == INT_MAX-1 );
static_assert( std::midpoint(INT_MAX, INT_MAX-2) == INT_MAX-1 );

static_assert( std::midpoint(0u, 0u) == 0 );
static_assert( std::midpoint(0u, 1u) == 0 );
static_assert( std::midpoint(1u, 0u) == 1 );
static_assert( std::midpoint(0u, 2u) == 1 );
static_assert( std::midpoint(3u, 2u) == 3 );
static_assert( std::midpoint(0u, UINT_MAX) == UINT_MAX/2 );
static_assert( std::midpoint(UINT_MAX, 0u) == (UINT_MAX/2 + 1) );
static_assert( std::midpoint(UINT_MAX, UINT_MAX) == UINT_MAX );
static_assert( std::midpoint(UINT_MAX, UINT_MAX-1) == UINT_MAX );
static_assert( std::midpoint(UINT_MAX-1, UINT_MAX-1) == UINT_MAX-1 );
static_assert( std::midpoint(UINT_MAX-1, UINT_MAX) == UINT_MAX-1 );
static_assert( std::midpoint(UINT_MAX, UINT_MAX-2) == UINT_MAX-1 );

static_assert( std::midpoint<short>(0, 0) == 0 );
static_assert( std::midpoint<short>(0, 1) == 0 );
static_assert( std::midpoint<short>(1, 0) == 1 );
static_assert( std::midpoint<short>(0, 2) == 1 );
static_assert( std::midpoint<short>(3, 2) == 3 );
static_assert( std::midpoint<short>(-5, 4) == -1 );
static_assert( std::midpoint<short>(5, -4) == 1 );
static_assert( std::midpoint<short>(-5, -4) == -5 );
static_assert( std::midpoint<short>(-4, -5) == -4 );
static_assert( std::midpoint<short>(SHRT_MIN, SHRT_MAX) == -1 );
static_assert( std::midpoint<short>(SHRT_MAX, SHRT_MIN) == 0 );
static_assert( std::midpoint<short>(SHRT_MAX, SHRT_MAX) == SHRT_MAX );
static_assert( std::midpoint<short>(SHRT_MAX, SHRT_MAX-1) == SHRT_MAX );
static_assert( std::midpoint<short>(SHRT_MAX-1, SHRT_MAX-1) == SHRT_MAX-1 );
static_assert( std::midpoint<short>(SHRT_MAX-1, SHRT_MAX) == SHRT_MAX-1 );
static_assert( std::midpoint<short>(SHRT_MAX, SHRT_MAX-2) == SHRT_MAX-1 );

static_assert( std::midpoint<signed char>(0, 0) == 0 );
static_assert( std::midpoint<signed char>(1, 1) == 1 );
static_assert( std::midpoint<signed char>(0, 1) == 0 );
static_assert( std::midpoint<signed char>(1, 0) == 1 );
static_assert( std::midpoint<signed char>(0, 2) == 1 );
static_assert( std::midpoint<signed char>(3, 2) == 3 );
static_assert( std::midpoint<signed char>(-5, 4) == -1 );
static_assert( std::midpoint<signed char>(5, -4) == 1 );
static_assert( std::midpoint<signed char>(-5, -4) == -5 );
static_assert( std::midpoint<signed char>(-4, -5) == -4 );
static_assert( std::midpoint<signed char>(SCHAR_MIN, SCHAR_MAX) == -1 );
static_assert( std::midpoint<signed char>(SCHAR_MAX, SCHAR_MIN) == 0 );
static_assert( std::midpoint<signed char>(SCHAR_MAX, SCHAR_MAX) == SCHAR_MAX );
static_assert( std::midpoint<signed char>(SCHAR_MAX, SCHAR_MAX-1) == SCHAR_MAX);

void
test01()
{
  // Test every possibility for signed char.
  for (int a = SCHAR_MIN; a <= SCHAR_MAX; ++a)
    for (int b = SCHAR_MIN; b <= SCHAR_MAX; ++b)
      VERIFY( std::midpoint(a, b) == std::midpoint<int>(a, b) );
}

int main()
{
  test01();
}
