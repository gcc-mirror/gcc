// { dg-do compile }

// 2006-02-04  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2006-2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 5.2.1.7 conf_hyperg

#include <tr1/math.h>

void
test01()
{

  float af = 2.0F, cf = 3.0F, xf = 0.5F;
  double ad = 2.0, cd = 3.0, xd = 0.5;
  long double al = 2.0L, cl = 3.0L, xl = 0.5L;

  conf_hyperg(af, cf, xf);
  conf_hypergf(af, cf, xf);
  conf_hyperg(ad, cd, xd);
  conf_hyperg(al, cl, xl);
  conf_hypergl(al, cl, xl);

  return;
}

