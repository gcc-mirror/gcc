// Inspired by libstdc++/7680 & 26_numerics/c_math.cc, 2003-04-12 ljr

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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


// { dg-do link }
// { dg-options "-D_XOPEN_SOURCE" { target *-*-freebsd* } }

#include <cmath>

int
test01()
{
  float a = 1.f;
  float b;
  std::modf(a, &b);
  return 0;
}

int
test02 ()
{
  float a = 0.0f;
  float b __attribute__((unused)) = std::acos(a);
  return 0;
}

int
main()
{
  test01();
  test02();
  return 0;
}
