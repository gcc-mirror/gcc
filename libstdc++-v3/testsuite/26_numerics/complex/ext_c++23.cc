// Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

// { dg-do link { target c++23 } }

#include <stdfloat>
#include <complex>

template <typename T>
__attribute__((__noipa__)) void
test_functions(T *p, std::complex<T> *q)
{
  p[0] = std::abs(q[0]);
  p[1] = std::arg(q[1]);
  q[2] = std::cos(q[2]);
  q[3] = std::cosh(q[3]);
  q[4] = std::exp(q[4]);
  q[5] = std::log(q[5]);
  q[6] = std::sin(q[6]);
  q[7] = std::sinh(q[7]);
  q[8] = std::sqrt(q[8]);
  q[9] = std::tan(q[9]);
  q[10] = std::tanh(q[10]);
  q[11] = std::pow(q[11], q[19]);
  q[12] = std::acos(q[12]);
  q[13] = std::asin(q[13]);
  q[14] = std::atan(q[14]);
  q[15] = std::acosh(q[15]);
  q[16] = std::asinh(q[16]);
  q[17] = std::atanh(q[17]);
  q[18] = std::proj(q[18]);
}

int
main()
{
#if defined(__STDCPP_FLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::float16_t p[2] = {};
    std::complex<std::float16_t> q[20] = {};
    test_functions(p, q);
  }
#endif
#if defined(__STDCPP_FLOAT32_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::float32_t p[2] = {};
    std::complex<std::float32_t> q[20] = {};
    test_functions(p, q);
  }
#endif
#if defined(__STDCPP_FLOAT64_T__) && defined(_GLIBCXX_DOUBLE_IS_IEEE_BINARY64)
  {
    std::float64_t p[2] = {};
    std::complex<std::float64_t> q[20] = {};
    test_functions(p, q);
  }
#endif
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_LDOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  {
    std::float128_t p[2] = {};
    std::complex<std::float128_t> q[20] = {};
    test_functions(p, q);
  }
#endif
#if defined(__STDCPP_BFLOAT16_T__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
  {
    std::bfloat16_t p[2] = {};
    std::complex<std::bfloat16_t> q[20] = {};
    test_functions(p, q);
  }
#endif
}
