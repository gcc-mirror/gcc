// 2013-02-08  Edward Smith-Rowland <3dw4rd@verizon.net>
//
// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

// PR libstdc++/56216 - Crash of Bessel functions at x==0!

#include <testsuite_hooks.h>
#include <tr1/cmath>

void
test01()
{
  bool test __attribute__((unused)) = true;

  double j0 = std::tr1::cyl_bessel_j(0.0, 0.0);
  double i0 = std::tr1::cyl_bessel_i(0.0, 0.0);
  double j1 = std::tr1::cyl_bessel_j(1.0, 0.0);
  double i1 = std::tr1::cyl_bessel_i(1.0, 0.0);

  VERIFY(j0 == 1.0);
  VERIFY(i0 == 1.0);
  VERIFY(j1 == 0.0);
  VERIFY(i1 == 0.0);
}

int
main()
{
  test01();

  return 0;
}
