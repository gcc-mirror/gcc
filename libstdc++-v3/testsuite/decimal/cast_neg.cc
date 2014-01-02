// Copyright (C) 2009-2014 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-require-effective-target dfp }

// ISO/IEC TR 24733 doesn't say explicitly that the conversion from a
// decimal floating-point type to a generic float type is prohibited but
// it implies that in section 4.3 when it says "In C, objects of decimal
// floating-oint type can be converted to generic floating-point type by
// means of an explicit cast.  In C++ this is not possible."  Check that
// attempt to do a cast are flagged as errors.

#include <decimal/decimal>

using namespace std::decimal;

float f;
double d;
long double ld;
decimal32 d32;
decimal64 d64;
decimal128 d128;

void
foo (void)
{
  f = d32;			// { dg-error "error" }
  f = d64;			// { dg-error "error" }
  f = d128;			// { dg-error "error" }
  d = d32;			// { dg-error "error" }
  d = d64;			// { dg-error "error" }
  d = d128;			// { dg-error "error" }
  ld = d32;			// { dg-error "error" }
  ld = d64;			// { dg-error "error" }
  ld = d128;			// { dg-error "error" }

  f = (float)d32;		// { dg-error "error" }
  f = (float)d64;		// { dg-error "error" }
  f = (float)d128;		// { dg-error "error" }
  d = (double)d32;		// { dg-error "error" }
  d = (double)d64;		// { dg-error "error" }
  d = (double)d128;		// { dg-error "error" }
  ld = (long double)d32;	// { dg-error "error" }
  ld = (long double)d64;	// { dg-error "error" }
  ld = (long double)d128;	// { dg-error "error" }
}
