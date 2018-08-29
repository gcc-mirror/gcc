// { dg-do compile }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

namespace gnu
{
  double d1 = DBL_DIG;
  double d2 = DBL_EPSILON;
  double d3 = DBL_MANT_DIG;
  double d4 = DBL_MAX;
  double d5 = DBL_MAX_10_EXP;
  double d6 = DBL_MAX_EXP;
  double d7 = DBL_MIN;
  double d8 = DBL_MIN_10_EXP;
  double d9 = DBL_MIN_EXP;

  float f1 = FLT_DIG;
  float f2 = FLT_EPSILON;
  float f3 = FLT_MANT_DIG;
  float f4 = FLT_MAX;
  float f5 = FLT_MAX_10_EXP;
  float f6 = FLT_MAX_EXP;
  float f7 = FLT_MIN;
  float f8 = FLT_MIN_10_EXP;
  float f9 = FLT_MIN_EXP;

  long double ld1 = LDBL_DIG;
  long double ld2 = LDBL_EPSILON;
  long double ld3 = LDBL_MANT_DIG;
  long double ld4 = LDBL_MAX;
  long double ld5 = LDBL_MAX_10_EXP;
  long double ld6 = LDBL_MAX_EXP;
  long double ld7 = LDBL_MIN;
  long double ld8 = LDBL_MIN_10_EXP;
  long double ld9 = LDBL_MIN_EXP;
}
