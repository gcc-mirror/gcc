// Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++23 } }
// { dg-require-cmath "" }

#include <stdfloat>
#include <cmath>
#include <limits>
#include <testsuite_hooks.h>

template <typename T>
void
test ()
{
  using lim = std::numeric_limits<T>;
  int t0 = std::ilogb(T(4.0));
  VERIFY( t0 == 2 );
  int t1 = std::ilogb(lim::infinity());
  VERIFY( t1 == std::numeric_limits<int>::max() );
  int t2 = std::ilogb(-lim::infinity());
  VERIFY( t2 == std::numeric_limits<int>::max() );
}

int
main ()
{
#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::float16_t>();
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::float32_t>();
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
  test <std::float64_t>();
#endif
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_LDOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  test <std::float128_t>();
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  test <std::bfloat16_t>();
#endif
}
