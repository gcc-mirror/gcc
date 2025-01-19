// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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
// { dg-excess-errors "" { target uclibc } }

#include <cmath>

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
    fp_type_one f1 = 1.0;
    fp_type_two f2 = 3.0;
    int resi;
    bool res;

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
  test_c99_classify<float>();
  test_c99_classify<double>();
  test_c99_classify<long double>();
  test_c99_classify<float, double>();
  test_c99_classify<float, long double>();
  test_c99_classify<double, float>();
  test_c99_classify<double, long double>();
  test_c99_classify<long double, float>();
  test_c99_classify<long double, double>();
#endif
  return 0;
}
