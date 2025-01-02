// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.2.3  Conversion from integral type (decimal32).
// ISO/IEC TR 24733  3.2.3.3  Conversion from integral type (decimal64).
// ISO/IEC TR 24733  3.2.4.3  Conversion from integral type (decimal128).

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

void
conversion_from_integral_p32 ()
{
  decimal32 d;
  decimal32 from_si (1);
  decimal32 from_ui (2U);
  decimal32 from_sl (3L);
  decimal32 from_ul (4UL);
  decimal32 from_sll (5LL);
  decimal32 from_ull (6ULL);

  d++; VERIFY (from_si == d);
  d++; VERIFY (from_ui == d);
  d++; VERIFY (from_sl == d);
  d++; VERIFY (from_ul == d);
  d++; VERIFY (from_sll == d);
  d++; VERIFY (from_ull == d);

  from_si = 7;
  d++; VERIFY (from_si == d);
  from_ui = 8U;
  d++; VERIFY (from_ui == d);
  from_sl = 9L;
  d++; VERIFY (from_sl == d);
  from_ul = 10UL;
  d++; VERIFY (from_ul == d);
  from_sll = 11LL;
  d++; VERIFY (from_sll == d);
  from_ull = 12ULL;
  d++; VERIFY (from_ull == d);
}

void
conversion_from_integral_m32 ()
{
  decimal32 d;
  decimal32 from_si (-1);
  decimal32 from_sl (-2L);
  decimal32 from_sll (-3LL);

  d--; VERIFY (from_si == d);
  d--; VERIFY (from_sl == d);
  d--; VERIFY (from_sll == d);

  from_si = -4;
  d--; VERIFY (from_si == d);
  from_sl = -5L;
  d--; VERIFY (from_sl == d);
  from_sll = -6LL;
  d--; VERIFY (from_sll == d);
}

void
conversion_from_integral_p64 ()
{
  decimal64 d;
  decimal64 from_si (1);
  decimal64 from_ui (2U);
  decimal64 from_sl (3L);
  decimal64 from_ul (4UL);
  decimal64 from_sll (5LL);
  decimal64 from_ull (6ULL);

  d++; VERIFY (from_si == d);
  d++; VERIFY (from_ui == d);
  d++; VERIFY (from_sl == d);
  d++; VERIFY (from_ul == d);
  d++; VERIFY (from_sll == d);
  d++; VERIFY (from_ull == d);

  from_si = 7;
  d++; VERIFY (from_si == d);
  from_ui = 8U;
  d++; VERIFY (from_ui == d);
  from_sl = 9L;
  d++; VERIFY (from_sl == d);
  from_ul = 10UL;
  d++; VERIFY (from_ul == d);
  from_sll = 11LL;
  d++; VERIFY (from_sll == d);
  from_ull = 12ULL;
  d++; VERIFY (from_ull == d);
}

void
conversion_from_integral_m64 ()
{
  decimal64 d;
  decimal64 from_si (-1);
  decimal64 from_sl (-2L);
  decimal64 from_sll (-3LL);

  d--; VERIFY (from_si == d);
  d--; VERIFY (from_sl == d);
  d--; VERIFY (from_sll == d);

  from_si = -4;
  d--; VERIFY (from_si == d);
  from_sl = -5L;
  d--; VERIFY (from_sl == d);
  from_sll = -6LL;
  d--; VERIFY (from_sll == d);
}

void
conversion_from_integral_p128 ()
{
  decimal128 d;
  decimal128 from_si (1);
  decimal128 from_ui (2U);
  decimal128 from_sl (3L);
  decimal128 from_ul (4UL);
  decimal128 from_sll (5LL);
  decimal128 from_ull (6ULL);

  d++; VERIFY (from_si == d);
  d++; VERIFY (from_ui == d);
  d++; VERIFY (from_sl == d);
  d++; VERIFY (from_ul == d);
  d++; VERIFY (from_sll == d);
  d++; VERIFY (from_ull == d);

  from_si = 7;
  d++; VERIFY (from_si == d);
  from_ui = 8U;
  d++; VERIFY (from_ui == d);
  from_sl = 9L;
  d++; VERIFY (from_sl == d);
  from_ul = 10UL;
  d++; VERIFY (from_ul == d);
  from_sll = 11LL;
  d++; VERIFY (from_sll == d);
  from_ull = 12ULL;
}

void
conversion_from_integral_m128 ()
{
  decimal128 d;
  decimal128 from_si (-1);
  decimal128 from_sl (-2L);
  decimal128 from_sll (-3LL);

  d--; VERIFY (from_si == d);
  d--; VERIFY (from_sl == d);
  d--; VERIFY (from_sll == d);

  from_si = -4;
  d--; VERIFY (from_si == d);
  from_sl = -5L;
  d--; VERIFY (from_sl == d);
  from_sll = -6LL;
  d--; VERIFY (from_sll == d);
}

int
main ()
{
  conversion_from_integral_p32 ();
  conversion_from_integral_m32 ();
  conversion_from_integral_p64 ();
  conversion_from_integral_m64 ();
  conversion_from_integral_p128 ();
  conversion_from_integral_m128 ();
}
