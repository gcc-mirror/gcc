// { dg-do compile }

// 2006-02-04  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

// 5.2.1.6 comp_ellint_3

#include <tr1/cmath>

void
test01()
{

  float kf = 0.5F, nuf = 0.5F;
  double kd = 0.5, nud = 0.5;
  long double kl = 0.5L, nul = 0.5L;

  std::tr1::comp_ellint_3(kf, nuf);
  std::tr1::comp_ellint_3f(kf, nuf);
  std::tr1::comp_ellint_3(kd, nud);
  std::tr1::comp_ellint_3(kl, nul);
  std::tr1::comp_ellint_3l(kl, nul);

  return;
}

