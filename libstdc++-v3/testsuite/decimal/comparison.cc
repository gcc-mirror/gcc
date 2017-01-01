// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.9  Comparison operators.

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

decimal32 d32 (5);
decimal64 d64 (-10);
decimal128 d128 (25);
int si = -20;
unsigned int ui = 50;
long sl = -10;
unsigned long ul = 20;
long long sll = -25;
unsigned long long ull = 50;

void
compare_eq_32 (void)
{
  decimal32 a;

  a = si;   VERIFY (a == si);   VERIFY (si == a);
  a = ui;   VERIFY (a == ui);   VERIFY (ui == a);
  a = sl;   VERIFY (a == sl);   VERIFY (sl == a);
  a = ul;   VERIFY (a == ul);   VERIFY (ul == a);
  a = sll;  VERIFY (a == sll);  VERIFY (sll == a);
  a = ull;  VERIFY (a == ull);  VERIFY (ull == a);
  a = d32;  VERIFY (a == d32);  VERIFY (d32 == a);
  a = (decimal32)d64;  VERIFY (a == d64);  VERIFY (d64 == a);
  a = (decimal32)d128; VERIFY (a == d128); VERIFY (d128 == a);
}

void
compare_ne_32 (void)
{
  decimal32 a = 100;

  VERIFY (a != si);   VERIFY (si != a);
  VERIFY (a != ui);   VERIFY (ui != a);
  VERIFY (a != sl);   VERIFY (sl != a);
  VERIFY (a != ul);   VERIFY (ul != a);
  VERIFY (a != sll);  VERIFY (sll != a);
  VERIFY (a != ull);  VERIFY (ull != a);
  VERIFY (a != d32);  VERIFY (d32 != a);
  VERIFY (a != d64);  VERIFY (d64 != a);
  VERIFY (a != d128); VERIFY (d128 != a);
}

void
compare_lt_32 (void)
{
  decimal32 a = -100;

  VERIFY (a < si);
  VERIFY (a < ui);
  VERIFY (a < sl);
  VERIFY (a < ul);
  VERIFY (a < sll);
  VERIFY (a < ull);
  VERIFY (a < d32);
  VERIFY (a < d64);
  VERIFY (a < d128);

  a = 100;
  VERIFY (si < a);
  VERIFY (ui < a);
  VERIFY (sl < a);
  VERIFY (ul < a);
  VERIFY (sll < a);
  VERIFY (ull < a);
  VERIFY (d32 < a);
  VERIFY (d64 < a);
  VERIFY (d128 < a);
}

void
compare_le_32 (void)
{
  decimal32 a;

  a = si;   VERIFY (a <= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a <= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a <= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a <= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a <= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a <= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a <= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a <= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a <= d128); VERIFY (d128 <= a);

  a = -100;
  VERIFY (a <= si);
  VERIFY (a <= ui);
  VERIFY (a <= sl);
  VERIFY (a <= ul);
  VERIFY (a <= sll);
  VERIFY (a <= ull);
  VERIFY (a <= d32);
  VERIFY (a <= d64);
  VERIFY (a <= d128);

  a = 100;
  VERIFY (si <= a);
  VERIFY (ui <= a);
  VERIFY (sl <= a);
  VERIFY (ul <= a);
  VERIFY (sll <= a);
  VERIFY (ull <= a);
  VERIFY (d32 <= a);
  VERIFY (d64 <= a);
  VERIFY (d128 <= a);
}

void
compare_gt_32 (void)
{
  decimal32 a = 100;

  VERIFY (a > si);
  VERIFY (a > ui);
  VERIFY (a > sl);
  VERIFY (a > ul);
  VERIFY (a > sll);
  VERIFY (a > ull);
  VERIFY (a > d32);
  VERIFY (a > d64);
  VERIFY (a > d128);

  a = -100;
  VERIFY (si > a);
  VERIFY (ui > a);
  VERIFY (sl > a);
  VERIFY (ul > a);
  VERIFY (sll > a);
  VERIFY (ull > a);
  VERIFY (d32 > a);
  VERIFY (d64 > a);
  VERIFY (d128 > a);
}

void
compare_ge_32 (void)
{
  decimal32 a;

  a = si;   VERIFY (a >= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a >= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a >= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a >= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a >= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a >= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a >= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a >= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a >= d128); VERIFY (d128 <= a);

  a = 100;
  VERIFY (a >= si);
  VERIFY (a >= ui);
  VERIFY (a >= sl);
  VERIFY (a >= ul);
  VERIFY (a >= sll);
  VERIFY (a >= ull);
  VERIFY (a >= d32);
  VERIFY (a >= d64);
  VERIFY (a >= d128);

  a = -100;
  VERIFY (si >= a);
  VERIFY (ui >= a);
  VERIFY (sl >= a);
  VERIFY (ul >= a);
  VERIFY (sll >= a);
  VERIFY (ull >= a);
  VERIFY (d32 >= a);
  VERIFY (d64 >= a);
  VERIFY (d128 >= a);
}

void
compare_eq_64 (void)
{
  decimal64 a;

  a = si;   VERIFY (a == si);   VERIFY (si == a);
  a = ui;   VERIFY (a == ui);   VERIFY (ui == a);
  a = sl;   VERIFY (a == sl);   VERIFY (sl == a);
  a = ul;   VERIFY (a == ul);   VERIFY (ul == a);
  a = sll;  VERIFY (a == sll);  VERIFY (sll == a);
  a = ull;  VERIFY (a == ull);  VERIFY (ull == a);
  a = d32;  VERIFY (a == d32);  VERIFY (d32 == a);
  a = d64;  VERIFY (a == d64);  VERIFY (d64 == a);
  a = (decimal64)d128; VERIFY (a == d128); VERIFY (d128 == a);
}

void
compare_ne_64 (void)
{
  decimal64 a = 100;

  VERIFY (a != si);   VERIFY (si != a);
  VERIFY (a != ui);   VERIFY (ui != a);
  VERIFY (a != sl);   VERIFY (sl != a);
  VERIFY (a != ul);   VERIFY (ul != a);
  VERIFY (a != sll);  VERIFY (sll != a);
  VERIFY (a != ull);  VERIFY (ull != a);
  VERIFY (a != d32);  VERIFY (d32 != a);
  VERIFY (a != d64);  VERIFY (d64 != a);
  VERIFY (a != d128); VERIFY (d128 != a);
}

void
compare_lt_64 (void)
{
  decimal64 a = -100;

  VERIFY (a < si);
  VERIFY (a < ui);
  VERIFY (a < sl);
  VERIFY (a < ul);
  VERIFY (a < sll);
  VERIFY (a < ull);
  VERIFY (a < d32);
  VERIFY (a < d64);
  VERIFY (a < d128);

  a = 100;
  VERIFY (si < a);
  VERIFY (ui < a);
  VERIFY (sl < a);
  VERIFY (ul < a);
  VERIFY (sll < a);
  VERIFY (ull < a);
  VERIFY (d32 < a);
  VERIFY (d64 < a);
  VERIFY (d128 < a);
}

void
compare_le_64 (void)
{
  decimal64 a;

  a = si;   VERIFY (a <= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a <= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a <= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a <= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a <= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a <= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a <= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a <= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a <= d128); VERIFY (d128 <= a);

  a = -100;
  VERIFY (a <= si);
  VERIFY (a <= ui);
  VERIFY (a <= sl);
  VERIFY (a <= ul);
  VERIFY (a <= sll);
  VERIFY (a <= ull);
  VERIFY (a <= d32);
  VERIFY (a <= d64);
  VERIFY (a <= d128);

  a = 100;
  VERIFY (si <= a);
  VERIFY (ui <= a);
  VERIFY (sl <= a);
  VERIFY (ul <= a);
  VERIFY (sll <= a);
  VERIFY (ull <= a);
  VERIFY (d32 <= a);
  VERIFY (d64 <= a);
  VERIFY (d128 <= a);
}

void
compare_gt_64 (void)
{
  decimal64 a = 100;

  VERIFY (a > si);
  VERIFY (a > ui);
  VERIFY (a > sl);
  VERIFY (a > ul);
  VERIFY (a > sll);
  VERIFY (a > ull);
  VERIFY (a > d32);
  VERIFY (a > d64);
  VERIFY (a > d128);

  a = -100;
  VERIFY (si > a);
  VERIFY (ui > a);
  VERIFY (sl > a);
  VERIFY (ul > a);
  VERIFY (sll > a);
  VERIFY (ull > a);
  VERIFY (d32 > a);
  VERIFY (d64 > a);
  VERIFY (d128 > a);
}

void
compare_ge_64 (void)
{
  decimal64 a;

  a = si;   VERIFY (a >= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a >= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a >= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a >= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a >= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a >= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a >= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a >= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a >= d128); VERIFY (d128 <= a);

  a = 100;
  VERIFY (a >= si);
  VERIFY (a >= ui);
  VERIFY (a >= sl);
  VERIFY (a >= ul);
  VERIFY (a >= sll);
  VERIFY (a >= ull);
  VERIFY (a >= d32);
  VERIFY (a >= d64);
  VERIFY (a >= d128);

  a = -100;
  VERIFY (si >= a);
  VERIFY (ui >= a);
  VERIFY (sl >= a);
  VERIFY (ul >= a);
  VERIFY (sll >= a);
  VERIFY (ull >= a);
  VERIFY (d32 >= a);
  VERIFY (d64 >= a);
  VERIFY (d128 >= a);
}

void
compare_eq_128 (void)
{
  decimal128 a;

  a = si;   VERIFY (a == si);   VERIFY (si == a);
  a = ui;   VERIFY (a == ui);   VERIFY (ui == a);
  a = sl;   VERIFY (a == sl);   VERIFY (sl == a);
  a = ul;   VERIFY (a == ul);   VERIFY (ul == a);
  a = sll;  VERIFY (a == sll);  VERIFY (sll == a);
  a = ull;  VERIFY (a == ull);  VERIFY (ull == a);
  a = d32;  VERIFY (a == d32);  VERIFY (d32 == a);
  a = d64;  VERIFY (a == d64);  VERIFY (d64 == a);
  a = d128; VERIFY (a == d128); VERIFY (d128 == a);
}

void
compare_ne_128 (void)
{
  decimal128 a = 100;

  VERIFY (a != si);   VERIFY (si != a);
  VERIFY (a != ui);   VERIFY (ui != a);
  VERIFY (a != sl);   VERIFY (sl != a);
  VERIFY (a != ul);   VERIFY (ul != a);
  VERIFY (a != sll);  VERIFY (sll != a);
  VERIFY (a != ull);  VERIFY (ull != a);
  VERIFY (a != d32);  VERIFY (d32 != a);
  VERIFY (a != d64);  VERIFY (d64 != a);
  VERIFY (a != d128); VERIFY (d128 != a);
}

void
compare_lt_128 (void)
{
  decimal128 a = -100;

  VERIFY (a < si);
  VERIFY (a < ui);
  VERIFY (a < sl);
  VERIFY (a < ul);
  VERIFY (a < sll);
  VERIFY (a < ull);
  VERIFY (a < d32);
  VERIFY (a < d64);
  VERIFY (a < d128);

  a = 100;
  VERIFY (si < a);
  VERIFY (ui < a);
  VERIFY (sl < a);
  VERIFY (ul < a);
  VERIFY (sll < a);
  VERIFY (ull < a);
  VERIFY (d32 < a);
  VERIFY (d64 < a);
  VERIFY (d128 < a);
}

void
compare_le_128 (void)
{
  decimal128 a;

  a = si;   VERIFY (a <= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a <= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a <= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a <= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a <= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a <= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a <= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a <= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a <= d128); VERIFY (d128 <= a);

  a = -100;
  VERIFY (a <= si);
  VERIFY (a <= ui);
  VERIFY (a <= sl);
  VERIFY (a <= ul);
  VERIFY (a <= sll);
  VERIFY (a <= ull);
  VERIFY (a <= d32);
  VERIFY (a <= d64);
  VERIFY (a <= d128);

  a = 100;
  VERIFY (si <= a);
  VERIFY (ui <= a);
  VERIFY (sl <= a);
  VERIFY (ul <= a);
  VERIFY (sll <= a);
  VERIFY (ull <= a);
  VERIFY (d32 <= a);
  VERIFY (d64 <= a);
  VERIFY (d128 <= a);
}

void
compare_gt_128 (void)
{
  decimal128 a = 100;

  VERIFY (a > si);
  VERIFY (a > ui);
  VERIFY (a > sl);
  VERIFY (a > ul);
  VERIFY (a > sll);
  VERIFY (a > ull);
  VERIFY (a > d32);
  VERIFY (a > d64);
  VERIFY (a > d128);

  a = -100;
  VERIFY (si > a);
  VERIFY (ui > a);
  VERIFY (sl > a);
  VERIFY (ul > a);
  VERIFY (sll > a);
  VERIFY (ull > a);
  VERIFY (d32 > a);
  VERIFY (d64 > a);
  VERIFY (d128 > a);
}

void
compare_ge_128 (void)
{
  decimal128 a;

  a = si;   VERIFY (a >= si);   VERIFY (si <= a);
  a = ui;   VERIFY (a >= ui);   VERIFY (ui <= a);
  a = sl;   VERIFY (a >= sl);   VERIFY (sl <= a);
  a = ul;   VERIFY (a >= ul);   VERIFY (ul <= a);
  a = sll;  VERIFY (a >= sll);  VERIFY (sll <= a);
  a = ull;  VERIFY (a >= ull);  VERIFY (ull <= a);
  a = d32;  VERIFY (a >= d32);  VERIFY (d32 <= a);
  a = (decimal32)d64;  VERIFY (a >= d64);  VERIFY (d64 <= a);
  a = (decimal32)d128; VERIFY (a >= d128); VERIFY (d128 <= a);

  a = 100;
  VERIFY (a >= si);
  VERIFY (a >= ui);
  VERIFY (a >= sl);
  VERIFY (a >= ul);
  VERIFY (a >= sll);
  VERIFY (a >= ull);
  VERIFY (a >= d32);
  VERIFY (a >= d64);
  VERIFY (a >= d128);

  a = -100;
  VERIFY (si >= a);
  VERIFY (ui >= a);
  VERIFY (sl >= a);
  VERIFY (ul >= a);
  VERIFY (sll >= a);
  VERIFY (ull >= a);
  VERIFY (d32 >= a);
  VERIFY (d64 >= a);
  VERIFY (d128 >= a);
}

int
main ()
{
  compare_eq_32 ();
  compare_ne_32 ();
  compare_lt_32 ();
  compare_le_32 ();
  compare_gt_32 ();
  compare_ge_32 ();

  compare_eq_64 ();
  compare_ne_64 ();
  compare_lt_64 ();
  compare_le_64 ();
  compare_gt_64 ();
  compare_ge_64 ();

  compare_eq_128 ();
  compare_ne_128 ();
  compare_lt_128 ();
  compare_le_128 ();
  compare_gt_128 ();
  compare_ge_128 ();
}
