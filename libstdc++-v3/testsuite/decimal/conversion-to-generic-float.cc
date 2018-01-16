// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.6  Conversion to generic floating-point type.

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

void
conversion_to_generic_float_32 ()
{
  decimal32 d32(123);
  float f;
  double d;
  long double ld;

  f = decimal32_to_float (d32);
  VERIFY (f == 123.F);
  d = decimal32_to_double (d32);
  VERIFY (d == 123.);
  ld = decimal32_to_long_double (d32);
  VERIFY (ld == 123.L);

  d32++;
  f = decimal_to_float (d32);
  VERIFY (f == 124.F);
  d = decimal_to_double (d32);
  VERIFY (d == 124.);
  ld = decimal_to_long_double (d32);
  VERIFY (ld == 124.L);
}

void
conversion_to_generic_float_64 ()
{
  decimal64 d64(234);
  float f;
  double d;
  long double ld;

  f = decimal64_to_float (d64);
  VERIFY (f == 234.F);
  d = decimal64_to_double (d64);
  VERIFY (d == 234.);
  ld = decimal64_to_long_double (d64);
  VERIFY (ld == 234.L);

  d64++;
  f = decimal_to_float (d64);
  VERIFY (f == 235.F);
  d = decimal_to_double (d64);
  VERIFY (d == 235.);
  ld = decimal_to_long_double (d64);
  VERIFY (ld == 235.L);
}

void
conversion_to_generic_float_128 ()
{
  decimal128 d128(345);
  float f;
  double d;
  long double ld;

  f = decimal128_to_float (d128);
  VERIFY (f == 345.F);
  d = decimal128_to_double (d128);
  VERIFY (d == 345.);
  ld = decimal128_to_long_double (d128);
  VERIFY (ld == 345.L);

  d128++;
  f = decimal_to_float (d128);
  VERIFY (f == 346.F);
  d = decimal_to_double (d128);
  VERIFY (d == 346.);
  ld = decimal_to_long_double (d128);
  VERIFY (ld == 346.L);
}

int
main ()
{
  conversion_to_generic_float_32 ();
  conversion_to_generic_float_64 ();
  conversion_to_generic_float_128 ();
}
