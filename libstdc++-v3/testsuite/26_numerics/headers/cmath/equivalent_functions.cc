// Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <testsuite_hooks.h>

#include <type_traits>
#include <cmath>
// also include functions in global namespace
#include <math.h>

static_assert(std::is_same<decltype(std::acosf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::asinf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::atanf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::atan2f(0.0, 0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::ceilf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::cosf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::coshf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::expf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::fabsf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::floorf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::fmodf(0.0, 0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::frexpf(0.0, nullptr)), float>::value, "");
static_assert(std::is_same<decltype(std::ldexpf(0.0, 0)), float>::value, "");
static_assert(std::is_same<decltype(std::logf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::log10f(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::modff(0.0, nullptr)), float>::value, "");
static_assert(std::is_same<decltype(std::powf(0.0, 0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::sinf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::sinhf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::sqrtf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::tanf(0.0)), float>::value, "");
static_assert(std::is_same<decltype(std::tanhf(0.0)), float>::value, "");

static_assert(std::is_same<decltype(std::acosl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::asinl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::atanl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::atan2l(0.0, 0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::ceill(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::cosl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::coshl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::expl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::fabsl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::floorl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::fmodl(0.0, 0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::frexpl(0.0, nullptr)), long double>::value, "");
static_assert(std::is_same<decltype(std::ldexpl(0.0, 0)), long double>::value, "");
static_assert(std::is_same<decltype(std::logl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::log10l(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::modfl(0.0, nullptr)), long double>::value, "");
static_assert(std::is_same<decltype(std::powl(0.0, 0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::sinl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::sinhl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::sqrtl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::tanl(0.0)), long double>::value, "");
static_assert(std::is_same<decltype(std::tanhl(0.0)), long double>::value, "");

void
test_float_global()
{
  float x = 1.0f;

  int e1 = 0;
  int e2 = 0;
  float i1 = 0.0f;
  float i2 = 0.0f;

  VERIFY(::acosf(x) == std::acosf(x));
  VERIFY(::asinf(x) == std::asinf(x));
  VERIFY(::atanf(x) == std::atanf(x));
  VERIFY(::atan2f(x, x) == std::atan2f(x, x));
  VERIFY(::ceilf(x) == std::ceilf(x));
  VERIFY(::cosf(x) == std::cosf(x));
  VERIFY(::coshf(x) == std::coshf(x));
  VERIFY(::expf(x) == std::expf(x));
  VERIFY(::fabsf(x) == std::fabsf(x));
  VERIFY(::floorf(x) == std::floorf(x));
  VERIFY(::fmodf(x, x) == std::fmodf(x, x));
  VERIFY(::frexpf(x, &e1) == std::frexpf(x, &e2));
  VERIFY(e1 == e2);
  VERIFY(::ldexpf(x, 1) == std::ldexpf(x, 1));
  VERIFY(::logf(x) == std::logf(x));
  VERIFY(::log10f(x) == std::log10f(x));
  VERIFY(::modff(x, &i1) == std::modff(x, &i2));
  VERIFY(i1 == i2);
  VERIFY(::powf(x, x) == std::powf(x, x));
  VERIFY(::sinf(x) == std::sinf(x));
  VERIFY(::sinhf(x) == std::sinhf(x));
  VERIFY(::sqrtf(x) == std::sqrtf(x));
  VERIFY(::tanf(x) == std::tanf(x));
  VERIFY(::tanhf(x) == std::tanhf(x));
}

void
test_float_overload()
{
  float x = 1.0f;

  int e1 = 0;
  int e2 = 0;
  float i1 = 0.0f;
  float i2 = 0.0f;

  VERIFY(std::acos(x) == std::acosf(x));
  VERIFY(std::asin(x) == std::asinf(x));
  VERIFY(std::atan(x) == std::atanf(x));
  VERIFY(std::atan2(x, x) == std::atan2f(x, x));
  VERIFY(std::ceil(x) == std::ceilf(x));
  VERIFY(std::cos(x) == std::cosf(x));
  VERIFY(std::cosh(x) == std::coshf(x));
  VERIFY(std::exp(x) == std::expf(x));
  VERIFY(std::fabs(x) == std::fabsf(x));
  VERIFY(std::floor(x) == std::floorf(x));
  VERIFY(std::fmod(x, x) == std::fmodf(x, x));
  VERIFY(std::frexp(x, &e1) == std::frexpf(x, &e2));
  VERIFY(e1 == e2);
  VERIFY(std::ldexp(x, 1) == std::ldexpf(x, 1));
  VERIFY(std::log(x) == std::logf(x));
  VERIFY(std::log10(x) == std::log10f(x));
  VERIFY(std::modf(x, &i1) == std::modff(x, &i2));
  VERIFY(i1 == i2);
  VERIFY(std::pow(x, x) == std::powf(x, x));
  VERIFY(std::sin(x) == std::sinf(x));
  VERIFY(std::sinh(x) == std::sinhf(x));
  VERIFY(std::sqrt(x) == std::sqrtf(x));
  VERIFY(std::tan(x) == std::tanf(x));
  VERIFY(std::tanh(x) == std::tanhf(x));
}

void
test_long_double_global()
{
  long double x = 1.0L;

  int e1 = 0;
  int e2 = 0;
  long double i1 = 0.0L;
  long double i2 = 0.0L;

  VERIFY(::acosl(x) == std::acosl(x));
  VERIFY(::asinl(x) == std::asinl(x));
  VERIFY(::atanl(x) == std::atanl(x));
  VERIFY(::atan2l(x, x) == std::atan2l(x, x));
  VERIFY(::ceill(x) == std::ceill(x));
  VERIFY(::cosl(x) == std::cosl(x));
  VERIFY(::coshl(x) == std::coshl(x));
  VERIFY(::expl(x) == std::expl(x));
  VERIFY(::fabsl(x) == std::fabsl(x));
  VERIFY(::floorl(x) == std::floorl(x));
  VERIFY(::fmodl(x, x) == std::fmodl(x, x));
  VERIFY(::frexpl(x, &e1) == std::frexpl(x, &e2));
  VERIFY(e1 == e2);
  VERIFY(::ldexpl(x, 1) == std::ldexpl(x, 1));
  VERIFY(::logl(x) == std::logl(x));
  VERIFY(::log10l(x) == std::log10l(x));
  VERIFY(::modfl(x, &i1) == std::modfl(x, &i2));
  VERIFY(i1 == i2);
  VERIFY(::powl(x, x) == std::powl(x, x));
  VERIFY(::sinl(x) == std::sinl(x));
  VERIFY(::sinhl(x) == std::sinhl(x));
  VERIFY(::sqrtl(x) == std::sqrtl(x));
  VERIFY(::tanl(x) == std::tanl(x));
  VERIFY(::tanhl(x) == std::tanhl(x));
}

void
test_long_double_overload()
{
  long double x = 1.0L;

  int e1 = 0;
  int e2 = 0;
  long double i1 = 0.0L;
  long double i2 = 0.0L;

  VERIFY(std::acos(x) == std::acosl(x));
  VERIFY(std::asin(x) == std::asinl(x));
  VERIFY(std::atan(x) == std::atanl(x));
  VERIFY(std::atan2(x, x) == std::atan2l(x, x));
  VERIFY(std::ceil(x) == std::ceill(x));
  VERIFY(std::cos(x) == std::cosl(x));
  VERIFY(std::cosh(x) == std::coshl(x));
  VERIFY(std::exp(x) == std::expl(x));
  VERIFY(std::fabs(x) == std::fabsl(x));
  VERIFY(std::floor(x) == std::floorl(x));
  VERIFY(std::fmod(x, x) == std::fmodl(x, x));
  VERIFY(std::frexp(x, &e1) == std::frexpl(x, &e2));
  VERIFY(e1 == e2);
  VERIFY(std::ldexp(x, 1) == std::ldexpl(x, 1));
  VERIFY(std::log(x) == std::logl(x));
  VERIFY(std::log10(x) == std::log10l(x));
  VERIFY(std::modf(x, &i1) == std::modfl(x, &i2));
  VERIFY(i1 == i2);
  VERIFY(std::pow(x, x) == std::powl(x, x));
  VERIFY(std::sin(x) == std::sinl(x));
  VERIFY(std::sinh(x) == std::sinhl(x));
  VERIFY(std::sqrt(x) == std::sqrtl(x));
  VERIFY(std::tan(x) == std::tanl(x));
  VERIFY(std::tanh(x) == std::tanhl(x));
}

int
main ()
{
  test_float_global();
  test_float_overload();
  test_long_double_global();
  test_long_double_overload();
}
