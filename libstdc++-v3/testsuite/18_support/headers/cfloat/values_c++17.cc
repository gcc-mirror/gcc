// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <cfloat>
#include "values.cc"

namespace gnu17
{
  double d1 = DBL_DECIMAL_DIG;
  double d2 = DBL_HAS_SUBNORM;
  double d3 = DBL_TRUE_MIN;

  float f1 = FLT_DECIMAL_DIG;
  float f2 = FLT_HAS_SUBNORM;
  float f3 = FLT_TRUE_MIN;

  long double ld1 = LDBL_DECIMAL_DIG;
  long double ld2 = LDBL_HAS_SUBNORM;
  long double ld3 = LDBL_TRUE_MIN;
}
