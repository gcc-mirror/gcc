// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#include <complex>
#include <limits>
#include <testsuite_hooks.h>

namespace test
{
#ifdef _GLIBCXX_USE_C99_MATH_TR1
  using std::copysign;
#else
  bool copysign(float x, float y)
  { return __builtin_copysignf(x, y); }

  bool copysign(double x, double y)
  { return __builtin_copysign(x, y); }

  bool copysign(long double x, long double y)
  { return __builtin_copysignl(x, y); }
#endif
}

template<typename T>
bool eq(const std::complex<T>& x, const std::complex<T>& y)
{
  bool nan_reals = std::isnan(x.real()) && std::isnan(y.real());
  bool nan_imags = std::isnan(x.imag()) && std::isnan(y.imag());

  bool sign_reals
    = test::copysign(T(1), x.real()) == test::copysign(T(1), y.real());
  bool sign_imags
    = test::copysign(T(1), x.imag()) == test::copysign(T(1), y.imag());

  return ((x.real() == y.real() && sign_reals) || nan_reals)
    && ((x.imag() == y.imag() && sign_imags) || nan_imags);
}

void
test01()
{
  const double qnan = std::numeric_limits<double>::quiet_NaN();
  const double pinf = std::numeric_limits<double>::infinity();
  const double ninf = -pinf;

  std::complex<double> c00(0, 0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );

  const std::complex<double> c01(0, 1);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );

  const std::complex<double> c10(1, 0);
  VERIFY( eq( std::proj(c10)  , c10 ) );
  VERIFY( eq( std::proj(-c10) , -c10 ) );

  const std::complex<double> c12(1, 2);
  VERIFY( eq( std::proj(c12)  , c12 ) );
  VERIFY( eq( std::proj(-c12) , -c12 ) );

  const std::complex<double> c0q(0, qnan);
  VERIFY( eq( std::proj(c0q)  , c0q ) );
  VERIFY( eq( std::proj(-c0q) , -c0q ) );

  const std::complex<double> c1q(1, qnan);
  VERIFY( eq( std::proj(c1q)  , c1q ) );
  VERIFY( eq( std::proj(-c1q) , -c1q ) );

  const std::complex<double> cq0(qnan, 0);
  VERIFY( eq( std::proj(cq0)  , cq0 ) );
  VERIFY( eq( std::proj(-cq0) , -cq0 ) );

  const std::complex<double> cq1(qnan, 1);
  VERIFY( eq( std::proj(cq1)  , cq1 ) );
  VERIFY( eq( std::proj(-cq1) , -cq1 ) );

  const std::complex<double> cqq(qnan, qnan);
  VERIFY( eq( std::proj(cqq)  , cqq ) );
  VERIFY( eq( std::proj(-cqq) , -cqq ) );

  const std::complex<double> c0p(0, pinf);
  VERIFY( eq( std::proj(c0p)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c0p) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> c1p(1, pinf);
  VERIFY( eq( std::proj(c1p)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c1p) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cqp(qnan, pinf);
  VERIFY( eq( std::proj(cqp)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cqp) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cpp(pinf, pinf);
  VERIFY( eq( std::proj(cpp)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpp) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double>  c0n(0, ninf);
  VERIFY( eq( std::proj(c0n) , std::complex<double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c0n) , std::complex<double>(pinf, +0.0) ) );

  const std::complex<double> c1n(1, ninf);
  VERIFY( eq( std::proj(c1n)  , std::complex<double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c1n) , std::complex<double>(pinf, +0.0) ) );

  const std::complex<double> cqn(qnan, ninf);
  VERIFY( eq( std::proj(cqn)  , std::complex<double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cqn) , std::complex<double>(pinf, +0.0) ) );

  const std::complex<double> cpn(pinf, ninf);
  VERIFY( eq( std::proj(cpn)  , std::complex<double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cpn) , std::complex<double>(pinf, +0.0) ) );

  const std::complex<double> cnn(ninf, ninf);
  VERIFY( eq( std::proj(cnn)  , std::complex<double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cnn) , std::complex<double>(pinf, +0.0) ) );

  const std::complex<double> cp0(pinf, 0);
  VERIFY( eq( std::proj(cp0)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp0) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cp1(pinf, 1);
  VERIFY( eq( std::proj(cp1)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp1) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cpq(pinf, qnan);
  VERIFY( eq( std::proj(cpq)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpq) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cn0(ninf, 0);
  VERIFY( eq( std::proj(cn0)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn0) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cn1(ninf, 1);
  VERIFY( eq( std::proj(cn1)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn1) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cnq(ninf, qnan);
  VERIFY( eq( std::proj(cnq)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnq) , std::complex<double>(pinf, -0.0) ) );

  const std::complex<double> cnp(ninf, pinf);
  VERIFY( eq( std::proj(cnp)  , std::complex<double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnp) , std::complex<double>(pinf, -0.0) ) );
}

void
test02()
{
  const float qnan = std::numeric_limits<float>::quiet_NaN();
  const float pinf = std::numeric_limits<float>::infinity();
  const float ninf = -pinf;

  std::complex<float> c00(0, 0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );

  const std::complex<float> c01(0, 1);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );

  const std::complex<float> c10(1, 0);
  VERIFY( eq( std::proj(c10)  , c10 ) );
  VERIFY( eq( std::proj(-c10) , -c10 ) );

  const std::complex<float> c12(1, 2);
  VERIFY( eq( std::proj(c12)  , c12 ) );
  VERIFY( eq( std::proj(-c12) , -c12 ) );

  const std::complex<float> c0q(0, qnan);
  VERIFY( eq( std::proj(c0q)  , c0q ) );
  VERIFY( eq( std::proj(-c0q) , -c0q ) );

  const std::complex<float> c1q(1, qnan);
  VERIFY( eq( std::proj(c1q)  , c1q ) );
  VERIFY( eq( std::proj(-c1q) , -c1q ) );

  const std::complex<float> cq0(qnan, 0);
  VERIFY( eq( std::proj(cq0)  , cq0 ) );
  VERIFY( eq( std::proj(-cq0) , -cq0 ) );

  const std::complex<float> cq1(qnan, 1);
  VERIFY( eq( std::proj(cq1)  , cq1 ) );
  VERIFY( eq( std::proj(-cq1) , -cq1 ) );

  const std::complex<float> cqq(qnan, qnan);
  VERIFY( eq( std::proj(cqq)  , cqq ) );
  VERIFY( eq( std::proj(-cqq) , -cqq ) );

  const std::complex<float> c0p(0, pinf);
  VERIFY( eq( std::proj(c0p)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c0p) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> c1p(1, pinf);
  VERIFY( eq( std::proj(c1p)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c1p) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cqp(qnan, pinf);
  VERIFY( eq( std::proj(cqp)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cqp) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cpp(pinf, pinf);
  VERIFY( eq( std::proj(cpp)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpp) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float>  c0n(0, ninf);
  VERIFY( eq( std::proj(c0n)  , std::complex<float>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c0n) , std::complex<float>(pinf, +0.0) ) );

  const std::complex<float> c1n(1, ninf);
  VERIFY( eq( std::proj(c1n)  , std::complex<float>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c1n) , std::complex<float>(pinf, +0.0) ) );

  const std::complex<float> cqn(qnan, ninf);
  VERIFY( eq( std::proj(cqn)  , std::complex<float>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cqn) , std::complex<float>(pinf, +0.0) ) );

  const std::complex<float> cpn(pinf, ninf);
  VERIFY( eq( std::proj(cpn)  , std::complex<float>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cpn) , std::complex<float>(pinf, +0.0) ) );

  const std::complex<float> cnn(ninf, ninf);
  VERIFY( eq( std::proj(cnn)  , std::complex<float>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cnn) , std::complex<float>(pinf, +0.0) ) );

  const std::complex<float> cp0(pinf, 0);
  VERIFY( eq( std::proj(cp0)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp0) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cp1(pinf, 1);
  VERIFY( eq( std::proj(cp1)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp1) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cpq(pinf, qnan);
  VERIFY( eq( std::proj(cpq)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpq) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cn0(ninf, 0);
  VERIFY( eq( std::proj(cn0)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn0) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cn1(ninf, 1);
  VERIFY( eq( std::proj(cn1)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn1) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cnq(ninf, qnan);
  VERIFY( eq( std::proj(cnq)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnq) , std::complex<float>(pinf, -0.0) ) );

  const std::complex<float> cnp(ninf, pinf);
  VERIFY( eq( std::proj(cnp)  , std::complex<float>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnp) , std::complex<float>(pinf, -0.0) ) );
}

void
test03()
{
  const long double qnan = std::numeric_limits<long double>::quiet_NaN();
  const long double pinf = std::numeric_limits<long double>::infinity();
  const long double ninf = -pinf;

  std::complex<long double> c00(0, 0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c00)  , c00 ) );
  VERIFY( eq( std::proj(-c00) , -c00 ) );

  const std::complex<long double> c01(0, 1);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );
  c00.real(-0.0);
  VERIFY( eq( std::proj(c01)  , c01 ) );
  VERIFY( eq( std::proj(-c01) , -c01 ) );

  const std::complex<long double> c10(1, 0);
  VERIFY( eq( std::proj(c10)  , c10 ) );
  VERIFY( eq( std::proj(-c10) , -c10 ) );

  const std::complex<long double> c12(1, 2);
  VERIFY( eq( std::proj(c12)  , c12 ) );
  VERIFY( eq( std::proj(-c12) , -c12 ) );

  const std::complex<long double> c0q(0, qnan);
  VERIFY( eq( std::proj(c0q)  , c0q ) );
  VERIFY( eq( std::proj(-c0q) , -c0q ) );

  const std::complex<long double> c1q(1, qnan);
  VERIFY( eq( std::proj(c1q)  , c1q ) );
  VERIFY( eq( std::proj(-c1q) , -c1q ) );

  const std::complex<long double> cq0(qnan, 0);
  VERIFY( eq( std::proj(cq0)  , cq0 ) );
  VERIFY( eq( std::proj(-cq0) , -cq0 ) );

  const std::complex<long double> cq1(qnan, 1);
  VERIFY( eq( std::proj(cq1)  , cq1 ) );
  VERIFY( eq( std::proj(-cq1) , -cq1 ) );

  const std::complex<long double> cqq(qnan, qnan);
  VERIFY( eq( std::proj(cqq)  , cqq ) );
  VERIFY( eq( std::proj(-cqq) , -cqq ) );

  const std::complex<long double> c0p(0, pinf);
  VERIFY( eq( std::proj(c0p)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c0p) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> c1p(1, pinf);
  VERIFY( eq( std::proj(c1p)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-c1p) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cqp(qnan, pinf);
  VERIFY( eq( std::proj(cqp)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cqp) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cpp(pinf, pinf);
  VERIFY( eq( std::proj(cpp)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpp) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double>  c0n(0, ninf);
  VERIFY( eq( std::proj(c0n) , std::complex<long double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c0n) , std::complex<long double>(pinf, +0.0) ) );

  const std::complex<long double> c1n(1, ninf);
  VERIFY( eq( std::proj(c1n)  , std::complex<long double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-c1n) , std::complex<long double>(pinf, +0.0) ) );

  const std::complex<long double> cqn(qnan, ninf);
  VERIFY( eq( std::proj(cqn)  , std::complex<long double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cqn) , std::complex<long double>(pinf, +0.0) ) );

  const std::complex<long double> cpn(pinf, ninf);
  VERIFY( eq( std::proj(cpn)  , std::complex<long double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cpn) , std::complex<long double>(pinf, +0.0) ) );

  const std::complex<long double> cnn(ninf, ninf);
  VERIFY( eq( std::proj(cnn)  , std::complex<long double>(pinf, -0.0) ) );
  VERIFY( eq( std::proj(-cnn) , std::complex<long double>(pinf, +0.0) ) );

  const std::complex<long double> cp0(pinf, 0);
  VERIFY( eq( std::proj(cp0)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp0) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cp1(pinf, 1);
  VERIFY( eq( std::proj(cp1)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cp1) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cpq(pinf, qnan);
  VERIFY( eq( std::proj(cpq)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cpq) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cn0(ninf, 0);
  VERIFY( eq( std::proj(cn0)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn0) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cn1(ninf, 1);
  VERIFY( eq( std::proj(cn1)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cn1) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cnq(ninf, qnan);
  VERIFY( eq( std::proj(cnq)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnq) , std::complex<long double>(pinf, -0.0) ) );

  const std::complex<long double> cnp(ninf, pinf);
  VERIFY( eq( std::proj(cnp)  , std::complex<long double>(pinf, +0.0) ) );
  VERIFY( eq( std::proj(-cnp) , std::complex<long double>(pinf, -0.0) ) );
}

int
main()
{
  test01();
  test02();
  test03();
}
