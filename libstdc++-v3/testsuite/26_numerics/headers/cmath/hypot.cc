// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <cmath>
#include <type_traits>
#if defined(__TEST_DEBUG)
#include <iostream>
#define VERIFY(A) \
if (!(A)) \
  { \
    std::cout << "line " << __LINE__ \
      << "  max_abs_frac = " << max_abs_frac \
      << "  tolerance = " << toler \
      << std::endl; \
  }
#else
#include <testsuite_hooks.h>
#endif

using std::is_same_v;
static_assert(is_same_v<double, decltype(std::hypot(0.0, 0.0, 0.0))>);
static_assert(is_same_v<double, decltype(std::hypot(0.0f, 0.0, 0.0))>);
static_assert(is_same_v<double, decltype(std::hypot(0.0, 0.0f, 0.0))>);
static_assert(is_same_v<double, decltype(std::hypot(0.0, 0.0, 0.0f))>);
static_assert(is_same_v<double, decltype(std::hypot(0.0f, 0.0f, 0.0))>);
static_assert(is_same_v<double, decltype(std::hypot(0.0f, 0.0, 0))>);
static_assert(is_same_v<long double, decltype(std::hypot(0.0f, 0.0, 0.0l))>);
static_assert(is_same_v<long double, decltype(std::hypot(0, 0.0, 0.0l))>);

template<typename T> struct testcase_hypot { T x, y, z, f0; };

template<typename Tp, unsigned int Num>
  void
  test(const testcase_hypot<Tp> (&data)[Num], Tp toler)
  {
    const Tp eps = std::numeric_limits<Tp>::epsilon();
    Tp max_abs_diff = -Tp(1);
    Tp max_abs_frac = -Tp(1);
    unsigned int num_datum = Num;
    for (unsigned int i = 0; i < num_datum; ++i)
      {
	const Tp f = std::hypot(data[i].x, data[i].y, data[i].z);
	const Tp f0 = data[i].f0;
	const Tp diff = f - f0;
	if (std::abs(diff) > max_abs_diff)
	  max_abs_diff = std::abs(diff);
	if (std::abs(f0) > Tp(10) * eps && std::abs(f) > Tp(10) * eps)
	  {
	    const Tp frac = diff / f0;
	    if (std::abs(frac) > max_abs_frac)
	      max_abs_frac = std::abs(frac);
	  }
      }
    VERIFY(max_abs_frac < toler);
  }

const testcase_hypot<double> data1[] = {
  { 0.0, 0.0, 0.0, 0.0 },
  { 0.0, 1.0, 1.0, std::sqrt(2.0) },
  { 1.0, 1.0, 1.0, std::sqrt(3.0) },
  { 1.0, 2.0, 2.0, 3.0 },
  { 2.0, 3.0, 6.0, 7.0 },
  { 1.0, 4.0, 8.0, 9.0 },
  { 4.0, 4.0, 7.0, 9.0 },
  { 12.0, 16.0, 21.0, 29.0 },
  { 1e8, 1., 1e-8, 1e8 },
  { 1., 1e8, 1e-8, 1e8 },
  { 1e-8, 1., 1e8, 1e8 },
  { 1e-2, 1e-4, 1e-4, 0.01000099995 },
  { 214748364., 214748364., 214748364., 371955077.2902952 }
};
const double toler1 = 1e-12;

const testcase_hypot<float> data2[] = {
  { 0.0f, 0.0f, 0.0f, 0.0f },
  { 0.0f, 1.0f, 1.0f, std::sqrt(2.0f) },
  { 1.0f, 1.0f, 1.0f, std::sqrt(3.0f) },
  { 1.0f, 2.0f, 2.0f, 3.0f },
  { 2.0f, 3.0f, 6.0f, 7.0f },
  { 1.0f, 4.0f, 8.0f, 9.0f },
  { 4.0f, 4.0f, 7.0f, 9.0f },
  { 12.0f, 16.0f, 21.0f, 29.0f },
  { 1e8f, 1.f, 1e-8f, 1e8f },
  { 1.f, 1e8f, 1e-8f, 1e8f },
  { 1e-8f, 1.f, 1e8f, 1e8f },
  { 1e-2f, 1e-4f, 1e-4f, 0.010001f },
  { 214748364.f, 214748364.f, 214748364.f, 371955072.f }
};
const float toler2 = 1e-7f;

const testcase_hypot<long double> data3[] = {
  { 0.0l, 0.0l, 0.0l, 0.0l },
  { 0.0l, 1.0l, 1.0l, std::sqrt(2.0l) },
  { 1.0l, 1.0l, 1.0l, std::sqrt(3.0l) },
  { 1.0l, 2.0l, 2.0l, 3.0l },
  { 2.0l, 3.0l, 6.0l, 7.0l },
  { 1.0l, 4.0l, 8.0l, 9.0l },
  { 4.0l, 4.0l, 7.0l, 9.0l },
  { 12.0l, 16.0l, 21.0l, 29.0l },
  { 1e8l, 1.l, 1e-8l, 1e8l },
  { 1.l, 1e8l, 1e-8l, 1e8l },
  { 1e-8l, 1.l, 1e8l, 1e8l },
  { 1e-2l, 1e-4l, 1e-4l, 0.010000999950004999375l },
  { 2147483647.l, 2147483647.l, 2147483647.l, 3719550785.027307813987l }
};
const long double toler3 = 1e-16l;

void
test01()
{
  test(data1, toler1);
  test(data2, toler2);
  if (sizeof(long double) > sizeof(double))
    test(data3, toler3);
  else
    test(data3, (long double)toler1);
}

int
main()
{
  test01();
}
