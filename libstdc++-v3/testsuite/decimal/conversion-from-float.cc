// Copyright (C) 2009 Free Software Foundation, Inc.
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

// { dg-require-effective-target dfp }

// ISO/IEC TR 24733  3.2.2.2  Conversion from floating-point type (decimal32).
// ISO/IEC TR 24733  3.2.3.2  Conversion from floating-point type (decimal64).
// ISO/IEC TR 24733  3.2.4.2  Conversion from floating-point type (decimal128).

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

void
conversion_from_float_32 ()
{
  decimal32 d32(123);
  decimal64 d64(234);
  decimal128 d128(345);
  float f = 456.F;
  double d = 567.;
  long double ld = 678.L;

  d32 = (decimal32) d64;
  VERIFY (d32 == make_decimal32 (234LL, 0));
  d32 = (decimal32) d128;
  VERIFY (d32 == make_decimal32 (345LL, 0));
  d32 = (decimal32) f;
  VERIFY (d32 == make_decimal32 (456LL, 0));
  d32 = (decimal32) d;
  VERIFY (d32 == make_decimal32 (567LL, 0));
  d32 = (decimal32) ld;
  VERIFY (d32 == make_decimal32 (678LL, 0));
}

void
conversion_from_float_64 ()
{
  decimal32 d32(123);
  decimal64 d64(234);
  decimal128 d128(345);
  float f = 456.F;
  double d = 567.;
  long double ld = 678.L;

  d64 = d32;
  VERIFY (d64 == make_decimal64 (123LL, 0));
  d64 = (decimal64) d128;
  VERIFY (d64 == make_decimal64 (345LL, 0));
  d64 = (decimal64) f;
  VERIFY (d64 == make_decimal64 (456LL, 0));
  d64 = (decimal64) d;
  VERIFY (d64 == make_decimal64 (567LL, 0));
  d64 = (decimal64) ld;
  VERIFY (d64 == make_decimal64 (678LL, 0));
}

void
conversion_from_float_128 ()
{
  decimal32 d32(123);
  decimal64 d64(234);
  decimal128 d128(345);
  float f = 456.F;
  double d = 567.;
  long double ld = 678.L;

  d128 = d32;
  VERIFY (d128 == make_decimal128 (123LL, 0));
  d128 = d64;
  VERIFY (d128 == make_decimal128 (234LL, 0));
  d128 = (decimal128) f;
  VERIFY (d128 == make_decimal128 (456LL, 0));
  d128 = (decimal128) d;
  VERIFY (d128 == make_decimal128 (567LL, 0));
  d128 = (decimal128) ld;
  VERIFY (d128 == make_decimal128 (678LL, 0));
}

int
main ()
{
  conversion_from_float_32 ();
  conversion_from_float_64 ();
  conversion_from_float_128 ();
}
