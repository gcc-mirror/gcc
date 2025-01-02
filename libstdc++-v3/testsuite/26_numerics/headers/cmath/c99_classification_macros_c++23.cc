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
// { dg-excess-errors "" { target uclibc } }

#include <cmath>
#include <stdfloat>

void fpclassify() { }

void isfinite() { }

void isinf() { }

void isnan() { }

void isnormal() { }

void signbit() { }

void isgreater() { }

void isgreaterequal() { }

void isless() { }

void islessequal() { }

void islessgreater() { }

void isunordered() { }

#if _GLIBCXX_USE_C99_MATH
template <typename _Tp, typename _Up = _Tp>
  void test_c99_classify()
  {
    typedef _Tp fp_type_one;
    typedef _Up fp_type_two;
    fp_type_one f1 = _Tp(1.0);
    fp_type_two f2 = _Up(3.0);
    int resi;
    volatile bool res;

    resi = std::fpclassify(f1);
    res = std::isfinite(f2);
    res = std::isinf(f1);
    res = std::isnan(f2);
    res = std::isnormal(f1);
    res = std::signbit(f2);
    res = std::isgreater(f1, f2);
    res = std::isgreaterequal(f1, f2);
    res = std::isless(f1, f2);
    res = std::islessequal(f1,f2);
    res = std::islessgreater(f1, f2);
    res = std::isunordered(f1, f2);
    resi = resi; // Suppress unused warning.
    res = res;
  }
#endif

int main()
{
#if _GLIBCXX_USE_C99_MATH
#ifdef __STDCPP_FLOAT16_T__
  test_c99_classify<std::float16_t>();
#endif
#ifdef __STDCPP_FLOAT32_T__
  test_c99_classify<std::float32_t>();
#endif
#ifdef __STDCPP_FLOAT64_T__
  test_c99_classify<std::float64_t>();
#endif
#ifdef __STDCPP_FLOAT128_T__
  test_c99_classify<std::float128_t>();
#endif
#ifdef __STDCPP_BFLOAT16_T__
  test_c99_classify<std::bfloat16_t>();
#endif
#endif
  return 0;
}
