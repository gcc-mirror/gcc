// { dg-do compile }

// 2006-02-04  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2006-2020 Free Software Foundation, Inc.
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

// 5.2.1.12 ellint_1

#include <tr1/cmath>

void
test01()
{
  float kf = 0.5F, phif = std::atan2(1.0F, 1.0F);
  double kd = 0.5, phid = std::atan2(1.0, 1.0);
  long double kl = 0.5L, phil = std::atan2(1.0L, 1.0L);

  std::tr1::ellint_1(kf, phif);
  std::tr1::ellint_1f(kf, phif);
  std::tr1::ellint_1(kd, phid);
  std::tr1::ellint_1(kl, phil);
  std::tr1::ellint_1l(kl, phil);

  return;
}

