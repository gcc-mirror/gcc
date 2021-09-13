// { dg-do compile }

// 2006-02-04  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2006-2021 Free Software Foundation, Inc.
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

// 5.2.1.3 beta

#include <tr1/math.h>

void
test01()
{

  float xf = 0.5F, yf = 0.5F;
  double xd = 0.5, yd = 0.5;
  long double xl = 0.5L, yl = 0.5L;

  beta(xf, yf);
  betaf(xf, yf);
  beta(xd, yd);
  beta(xl, yl);
  betal(xl, yl);

  return;
}

