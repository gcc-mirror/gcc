// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do run }

#include <valarray>
#include <testsuite_hooks.h>

bool eq(double d, double e)
{
  return std::abs(d - e) < 0.01;
}

void
test01()
{
  std::valarray<double> v(2);
  v[0] = -0.5;
  v[1] = 0.25;
  std::valarray<double> v_abs = abs(v);
  VERIFY( v_abs[0] == 0.5 );
  VERIFY( v_abs[1] == 0.25 );

  std::valarray<double> v_acos = acos(v);
  VERIFY( eq( v_acos[0], 2.09 ) );
  VERIFY( eq( v_acos[1], 1.31 ) );

  std::valarray<double> v_asin = asin(v);
  VERIFY( eq( v_asin[0], -0.52 ) );
  VERIFY( eq( v_asin[1], 0.25 ) );

  std::valarray<double> v_atan = atan(v);
  VERIFY( eq( v_atan[0], -0.46 ) );
  VERIFY( eq( v_atan[1], 0.24 ) );

  std::valarray<double> v2(2);
  v2[0] = 4;
  v2[1] = 3;
  std::valarray<double> v_atan2 = atan2(v, v2);
  VERIFY( eq( v_atan2[0], -0.12 ) );
  VERIFY( eq( v_atan2[1], 0.08 ) );

  v_atan2 = atan2(v, 4);  // LWG 3074 allows mixed types
  VERIFY( eq( v_atan2[0], -0.12 ) );
  VERIFY( eq( v_atan2[1], 0.06 ) );

  v_atan2 = atan2(4, v);  // LWG 3074 allows mixed types
  VERIFY( eq( v_atan2[0], 1.69 ) );
  VERIFY( eq( v_atan2[1], 1.50 ) );

  std::valarray<double> v_cos = cos(v);
  VERIFY( eq( v_cos[0], 0.87 ) );
  VERIFY( eq( v_cos[1], 0.96 ) );

  std::valarray<double> v_cosh = cosh(v);
  VERIFY( eq( v_cosh[0], 1.12 ) );
  VERIFY( eq( v_cosh[1], 1.03 ) );

  std::valarray<double> v_exp = exp(v);
  VERIFY( eq( v_exp[0], 0.60 ) );
  VERIFY( eq( v_exp[1], 1.28 ) );

  std::valarray<double> v_log = log(v);
  VERIFY( eq( v_log[1], -1.38 ) );

  std::valarray<double> v_log10 = log10(v);
  VERIFY( eq( v_log10[1], -0.60 ) );

  std::valarray<double> v_pow = pow(v, v2);
  VERIFY( eq( v_pow[0], 0.06 ) );
  VERIFY( eq( v_pow[1], 0.01 ) );

  v_pow = pow(v, 3);  // LWG 3074 allows mixed types
  VERIFY( eq( v_pow[0], -0.12 ) );
  VERIFY( eq( v_pow[1], 0.01 ) );

  v_pow = pow(4, v);  // LWG 3074 allows mixed types
  VERIFY( eq( v_pow[0], 0.5 ) );
  VERIFY( eq( v_pow[1], 1.41 ) );
}

int
main()
{
  test01();
}
