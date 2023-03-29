// { dg-options "-std=gnu++2b" }
// { dg-do compile { target c++23 } }

// Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

#include <stdfloat>

namespace gnu
{
#if defined(__STDCPP_FLOAT16_T__)
  typedef std::float16_t t1;
#endif
#if defined(__STDCPP_FLOAT32_T__)
  typedef std::float32_t t2;
#endif
#if defined(__STDCPP_FLOAT64_T__)
  typedef std::float64_t t3;
#endif
#if defined(__STDCPP_FLOAT128_T__)
  typedef std::float128_t t4;
#endif
#if defined(__STDCPP_BFLOAT16_T__)
  typedef std::bfloat16_t t5;
#endif
}
