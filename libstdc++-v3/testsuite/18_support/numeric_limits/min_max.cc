// { dg-add-options ieee }

// 1999-08-23 bkoz

// Copyright (C) 1999-2017 Free Software Foundation, Inc.
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <limits.h>
#include <float.h>
#include <cwchar>
#include <testsuite_hooks.h>

template<typename T>
struct extrema {
  static T min;
  static T max;
};


#define DEFINE_EXTREMA(T, m, M) \
  template<> T extrema<T>::min = m; \
  template<> T extrema<T>::max = M

DEFINE_EXTREMA(char, CHAR_MIN, CHAR_MAX);
DEFINE_EXTREMA(signed char, SCHAR_MIN, SCHAR_MAX);
DEFINE_EXTREMA(unsigned char, 0, UCHAR_MAX);
DEFINE_EXTREMA(short, SHRT_MIN, SHRT_MAX);
DEFINE_EXTREMA(unsigned short, 0, USHRT_MAX);
DEFINE_EXTREMA(int, INT_MIN, INT_MAX);
DEFINE_EXTREMA(unsigned, 0U, UINT_MAX);
DEFINE_EXTREMA(long, LONG_MIN, LONG_MAX);
DEFINE_EXTREMA(unsigned long, 0UL, ULONG_MAX);

#if _GLIBCXX_USE_WCHAR_T
DEFINE_EXTREMA(wchar_t, WCHAR_MIN, WCHAR_MAX);
#endif //_GLIBCXX_USE_WCHAR_T

DEFINE_EXTREMA(float, FLT_MIN, FLT_MAX);
DEFINE_EXTREMA(double, DBL_MIN, DBL_MAX);
DEFINE_EXTREMA(long double, LDBL_MIN, LDBL_MAX);

#undef DEFINE_EXTREMA

template<typename T>
void test_extrema()
{
  T limits_min = std::numeric_limits<T>::min();
  T limits_max = std::numeric_limits<T>::max();
  T extrema_min = extrema<T>::min;
  T extrema_max = extrema<T>::max;
  VERIFY( extrema_min == limits_min );
  VERIFY( extrema_max == limits_max );
}

int main()
{
  test_extrema<char>();
  test_extrema<signed char>();
  test_extrema<unsigned char>();
  
  test_extrema<short>();
  test_extrema<unsigned short>();

  test_extrema<int>();
  test_extrema<unsigned>();

  test_extrema<long>();
  test_extrema<unsigned long>();

  test_extrema<float>();
  test_extrema<double>();
  test_extrema<long double>();

  return 0;
}
