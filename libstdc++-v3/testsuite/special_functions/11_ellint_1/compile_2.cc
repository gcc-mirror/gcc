// { dg-do compile { target c++11 } }
// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__" }

// Copyright (C) 2016-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 8.1.11 ellint_1

#include <math.h>

void
test01()
{
  float kf = 0.5F, phif = atan2(1.0F, 1.0F);
  double kd = 0.5, phid = atan2(1.0, 1.0);
  long double kl = 0.5L, phil = atan2(1.0L, 1.0L);

  ellint_1(kf, phif);
  ellint_1f(kf, phif);
  ellint_1(kd, phid);
  ellint_1(kl, phil);
  ellint_1l(kl, phil);

  return;
}

