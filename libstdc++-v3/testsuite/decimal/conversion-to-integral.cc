// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.2.4  Conversion to integral type (decimal32).
// ISO/IEC TR 24733  3.2.3.4  Conversion to integral type (decimal64).
// ISO/IEC TR 24733  3.2.4.4  Conversion to integral type (decimal128).

#include <decimal/decimal>
#include <climits>
#include <testsuite_hooks.h>

// Use extension to replace implicit long long conversion with function call.
#define LONGLONG(X) decimal_to_long_long(X)

using namespace std::decimal;

void
conversion_to_integral_32 (void)
{
  #undef MAXVAL
  #define MAXVAL 999999LL
  decimal32 a, b (1), c (-1), d (MAXVAL), e (-MAXVAL);
  long long ll;

  ll = LONGLONG (a); VERIFY (ll == 0LL);
  ll = LONGLONG (b); VERIFY (ll == 1LL);
  ll = LONGLONG (c); VERIFY (ll == -1LL);
  ll = LONGLONG (d); VERIFY (ll == MAXVAL);
  ll = LONGLONG (e); VERIFY (ll == -MAXVAL);
}

void
conversion_to_integral_64 (void)
{
  #undef MAXVAL
  #define MAXVAL 999999999999999LL
  decimal64 a, b (1), c (-1), d (MAXVAL), e (-MAXVAL);
  long long ll;

  ll = LONGLONG (a); VERIFY (ll == 0LL);
  ll = LONGLONG (b); VERIFY (ll == 1LL);
  ll = LONGLONG (c); VERIFY (ll == -1LL);
  ll = LONGLONG (d); VERIFY (ll == MAXVAL);
  ll = LONGLONG (e); VERIFY (ll == -MAXVAL);
}

void
conversion_to_integral_128 (void)
{
  #undef MAXVAL
  #define MAXVAL __LONG_LONG_MAX__
  decimal128 a, b (1), c (-1), d (MAXVAL), e (-MAXVAL);
  long long ll;

  ll = LONGLONG (a); VERIFY (ll == 0LL);
  ll = LONGLONG (b); VERIFY (ll == 1LL);
  ll = LONGLONG (c); VERIFY (ll == -1LL);
  ll = LONGLONG (d); VERIFY (ll == MAXVAL);
  ll = LONGLONG (e); VERIFY (ll == -MAXVAL);
}

int
main ()
{
  conversion_to_integral_32 ();
  conversion_to_integral_64 ();
  conversion_to_integral_128 ();
}
