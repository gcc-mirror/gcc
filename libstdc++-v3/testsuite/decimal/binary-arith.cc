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

// { dg-require-effective-target dfp }

// ISO/IEC TR 24733  3.2.8  Binary arithmetic operators.

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
binary_add_32 (void)
{
  bool test __attribute__((unused)) = true;
  decimal32 a (1000), b;

  b = si + a;   VERIFY (b == 980);
  b = ui + a;   VERIFY (b == 1050);
  b = sl + a;   VERIFY (b == 990);
  b = ul + a;   VERIFY (b == 1020);
  b = sll + a;  VERIFY (b == 975);
  b = ull + a;  VERIFY (b == 1050);
  b = d32 + a;  VERIFY (b == 1005);
  b = (decimal32)(d64 + a);  VERIFY (b == 990);
  b = (decimal32)(d128 + a); VERIFY (b == 1025);

  b = a + si;   VERIFY (b == 980);
  b = a + ui;   VERIFY (b == 1050);
  b = a + sl;   VERIFY (b == 990);
  b = a + ul;   VERIFY (b == 1020);
  b = a + sll;  VERIFY (b == 975);
  b = a + ull;  VERIFY (b == 1050);
  b = a + d32;  VERIFY (b == 1005);
  b = (decimal32)(a + d64);  VERIFY (b == 990);
  b = (decimal32)(a + d128); VERIFY (b == 1025);
}

void
binary_subtract_32 (void)
{
  bool test __attribute__((unused)) = true;
  decimal32 a (1000), b;

  b = a - si;   VERIFY (b == 1020);
  b = a - ui;   VERIFY (b == 950);
  b = a - sl;   VERIFY (b == 1010);
  b = a - ul;   VERIFY (b == 980);
  b = a - sll;  VERIFY (b == 1025);
  b = a - ull;  VERIFY (b == 950);
  b = a - d32;  VERIFY (b == 995);
  b = (decimal32)(a - d64);  VERIFY (b == 1010);
  b = (decimal32)(a - d128); VERIFY (b == 975);

  a = -1000;
  b = si - a;   VERIFY (b == 980);
  b = ui - a;   VERIFY (b == 1050);
  b = sl - a;   VERIFY (b == 990);
  b = ul - a;   VERIFY (b == 1020);
  b = sll - a;  VERIFY (b == 975);
  b = ull - a;  VERIFY (b == 1050);
  b = d32 - a;  VERIFY (b == 1005);
  b = (decimal32)(d64 - a);  VERIFY (b == 990);
  b = (decimal32)(d128 - a); VERIFY (b == 1025);
}

void
binary_multiply_32 (void)
{
  bool test __attribute__((unused)) = true;
  decimal32 a (1000), b;

  b = a * si;   VERIFY (b == -20000);
  b = a * ui;   VERIFY (b == 50000);
  b = a * sl;   VERIFY (b == -10000);
  b = a * ul;   VERIFY (b == 20000);
  b = a * sll;  VERIFY (b == -25000);
  b = a * ull;  VERIFY (b == 50000);
  b = a * d32;  VERIFY (b == 5000);
  b = (decimal32)(a * d64);  VERIFY (b == -10000);
  b = (decimal32)(a * d128); VERIFY (b == 25000);

  b = si * a;   VERIFY (b == -20000);
  b = ui * a;   VERIFY (b == 50000);
  b = sl * a;   VERIFY (b == -10000);
  b = ul * a;   VERIFY (b == 20000);
  b = sll * a;  VERIFY (b == -25000);
  b = ull * a;  VERIFY (b == 50000);
  b = d32 * a;  VERIFY (b == 5000);
  b = (decimal32)(d64 * a);  VERIFY (b == -10000);
  b = (decimal32)(d128 * a); VERIFY (b == 25000);
}

void
binary_divide_32 (void)
{
  bool test __attribute__((unused)) = true;
  decimal32 a (1000), b;

  b = a / si;   VERIFY (b == -50);
  b = a / ui;   VERIFY (b == 20);
  b = a / sl;   VERIFY (b == -100);
  b = a / ul;   VERIFY (b == 50);
  b = a / sll;  VERIFY (b == -40);
  b = a / ull;  VERIFY (b == 20);
  b = a / d32;  VERIFY (b == 200);
  b = (decimal32)(a / d64);  VERIFY (b == -100);
  b = (decimal32)(a / d128); VERIFY (b == 40);

  a = 5;
  b = si / a;   VERIFY (b == -4);
  b = ui / a;   VERIFY (b == 10);
  b = sl / a;   VERIFY (b == -2);
  b = ul / a;   VERIFY (b == 4);
  b = sll / a;  VERIFY (b == -5);
  b = ull / a;  VERIFY (b == 10);
  b = d32 / a;  VERIFY (b == 1);
  b = (decimal32)(d64 / a);  VERIFY (b == -2);
  b = (decimal32)(d128 / a); VERIFY (b == 5);
}

void
binary_add_64 (void)
{
  bool test __attribute__((unused)) = true;
  decimal64 a (1000), b;

  b = a + si;   VERIFY (b == 980);
  b = a + ui;   VERIFY (b == 1050);
  b = a + sl;   VERIFY (b == 990);
  b = a + ul;   VERIFY (b == 1020);
  b = a + sll;  VERIFY (b == 975);
  b = a + ull;  VERIFY (b == 1050);
  b = a + d32;  VERIFY (b == 1005);
  b = a + d64;  VERIFY (b == 990);
  b = (decimal64)(a + d128); VERIFY (b == 1025);

  b = a + si;   VERIFY (b == 980);
  b = a + ui;   VERIFY (b == 1050);
  b = a + sl;   VERIFY (b == 990);
  b = a + ul;   VERIFY (b == 1020);
  b = a + sll;  VERIFY (b == 975);
  b = a + ull;  VERIFY (b == 1050);
  b = a + d32;  VERIFY (b == 1005);
  b = a + d64;  VERIFY (b == 990);
  b = (decimal64)(a + d128); VERIFY (b == 1025);
}

void
binary_subtract_64 (void)
{
  bool test __attribute__((unused)) = true;
  decimal64 a (1000), b;

  b = a - si;   VERIFY (b == 1020);
  b = a - ui;   VERIFY (b == 950);
  b = a - sl;   VERIFY (b == 1010);
  b = a - ul;   VERIFY (b == 980);
  b = a - sll;  VERIFY (b == 1025);
  b = a - ull;  VERIFY (b == 950);
  b = a - d32;  VERIFY (b == 995);
  b = a - d64;  VERIFY (b == 1010);
  b = (decimal64)(a - d128); VERIFY (b == 975);

  a = -1000;
  b = si - a;   VERIFY (b == 980);
  b = ui - a;   VERIFY (b == 1050);
  b = sl - a;   VERIFY (b == 990);
  b = ul - a;   VERIFY (b == 1020);
  b = sll - a;  VERIFY (b == 975);
  b = ull - a;  VERIFY (b == 1050);
  b = d32 - a;  VERIFY (b == 1005);
  b = d64 - a;  VERIFY (b == 990);
  b = (decimal64)(d128 - a); VERIFY (b == 1025);
}

void
binary_multiply_64 (void)
{
  bool test __attribute__((unused)) = true;
  decimal64 a (1000), b;

  b = a * si;   VERIFY (b == -20000);
  b = a * ui;   VERIFY (b == 50000);
  b = a * sl;   VERIFY (b == -10000);
  b = a * ul;   VERIFY (b == 20000);
  b = a * sll;  VERIFY (b == -25000);
  b = a * ull;  VERIFY (b == 50000);
  b = a * d32;  VERIFY (b == 5000);
  b = a * d64;  VERIFY (b == -10000);
  b = (decimal64)(a * d128); VERIFY (b == 25000);

  b = si * a;   VERIFY (b == -20000);
  b = ui * a;   VERIFY (b == 50000);
  b = sl * a;   VERIFY (b == -10000);
  b = ul * a;   VERIFY (b == 20000);
  b = sll * a;  VERIFY (b == -25000);
  b = ull * a;  VERIFY (b == 50000);
  b = d32 * a;  VERIFY (b == 5000);
  b = d64 * a;  VERIFY (b == -10000);
  b = (decimal64)(d128 * a); VERIFY (b == 25000);
}

void
binary_divide_64 (void)
{
  bool test __attribute__((unused)) = true;
  decimal64 a (1000), b;

  b = a / si;   VERIFY (b == -50);
  b = a / ui;   VERIFY (b == 20);
  b = a / sl;   VERIFY (b == -100);
  b = a / ul;   VERIFY (b == 50);
  b = a / sll;  VERIFY (b == -40);
  b = a / ull;  VERIFY (b == 20);
  b = a / d32;  VERIFY (b == 200);
  b = a / d64;  VERIFY (b == -100);
  b = (decimal64)(a / d128); VERIFY (b == 40);

  a = 5;
  b = si / a;   VERIFY (b == -4);
  b = ui / a;   VERIFY (b == 10);
  b = sl / a;   VERIFY (b == -2);
  b = ul / a;   VERIFY (b == 4);
  b = sll / a;  VERIFY (b == -5);
  b = ull / a;  VERIFY (b == 10);
  b = d32 / a;  VERIFY (b == 1);
  b = d64 / a;  VERIFY (b == -2);
  b = (decimal64)(d128 / a); VERIFY (b == 5);
}

void
binary_add_128 (void)
{
  bool test __attribute__((unused)) = true;
  decimal128 a (1000), b;

  b = a + si;   VERIFY (b == 980);
  b = a + ui;   VERIFY (b == 1050);
  b = a + sl;   VERIFY (b == 990);
  b = a + ul;   VERIFY (b == 1020);
  b = a + sll;  VERIFY (b == 975);
  b = a + ull;  VERIFY (b == 1050);
  b = a + d32;  VERIFY (b == 1005);
  b = a + d64;  VERIFY (b == 990);
  b = a + d128; VERIFY (b == 1025);

  b = a + si;   VERIFY (b == 980);
  b = a + ui;   VERIFY (b == 1050);
  b = a + sl;   VERIFY (b == 990);
  b = a + ul;   VERIFY (b == 1020);
  b = a + sll;  VERIFY (b == 975);
  b = a + ull;  VERIFY (b == 1050);
  b = a + d32;  VERIFY (b == 1005);
  b = a + d64;  VERIFY (b == 990);
  b = a + d128; VERIFY (b == 1025);
}

void
binary_subtract_128 (void)
{
  bool test __attribute__((unused)) = true;
  decimal128 a (1000), b;

  b = a - si;   VERIFY (b == 1020);
  b = a - ui;   VERIFY (b == 950);
  b = a - sl;   VERIFY (b == 1010);
  b = a - ul;   VERIFY (b == 980);
  b = a - sll;  VERIFY (b == 1025);
  b = a - ull;  VERIFY (b == 950);
  b = a - d32;  VERIFY (b == 995);
  b = a - d64;  VERIFY (b == 1010);
  b = a - d128; VERIFY (b == 975);

  a = -1000;
  b = si - a;   VERIFY (b == 980);
  b = ui - a;   VERIFY (b == 1050);
  b = sl - a;   VERIFY (b == 990);
  b = ul - a;   VERIFY (b == 1020);
  b = sll - a;  VERIFY (b == 975);
  b = ull - a;  VERIFY (b == 1050);
  b = d32 - a;  VERIFY (b == 1005);
  b = d64 - a;  VERIFY (b == 990);
  b = d128 - a; VERIFY (b == 1025);
}

void
binary_multiply_128 (void)
{
  bool test __attribute__((unused)) = true;
  decimal128 a (1000), b;

  b = a * si;   VERIFY (b == -20000);
  b = a * ui;   VERIFY (b == 50000);
  b = a * sl;   VERIFY (b == -10000);
  b = a * ul;   VERIFY (b == 20000);
  b = a * sll;  VERIFY (b == -25000);
  b = a * ull;  VERIFY (b == 50000);
  b = a * d32;  VERIFY (b == 5000);
  b = a * d64;  VERIFY (b == -10000);
  b = a * d128; VERIFY (b == 25000);

  b = si * a;   VERIFY (b == -20000);
  b = ui * a;   VERIFY (b == 50000);
  b = sl * a;   VERIFY (b == -10000);
  b = ul * a;   VERIFY (b == 20000);
  b = sll * a;  VERIFY (b == -25000);
  b = ull * a;  VERIFY (b == 50000);
  b = d32 * a;  VERIFY (b == 5000);
  b = d64 * a;  VERIFY (b == -10000);
  b = d128 * a; VERIFY (b == 25000);
}

void
binary_divide_128 (void)
{
  bool test __attribute__((unused)) = true;
  decimal128 a (1000), b;

  b = a / si;   VERIFY (b == -50);
  b = a / ui;   VERIFY (b == 20);
  b = a / sl;   VERIFY (b == -100);
  b = a / ul;   VERIFY (b == 50);
  b = a / sll;  VERIFY (b == -40);
  b = a / ull;  VERIFY (b == 20);
  b = a / d32;  VERIFY (b == 200);
  b = a / d64;  VERIFY (b == -100);
  b = a / d128; VERIFY (b == 40);

  a = 5;
  b = si / a;   VERIFY (b == -4);
  b = ui / a;   VERIFY (b == 10);
  b = sl / a;   VERIFY (b == -2);
  b = ul / a;   VERIFY (b == 4);
  b = sll / a;  VERIFY (b == -5);
  b = ull / a;  VERIFY (b == 10);
  b = d32 / a;  VERIFY (b == 1);
  b = d64 / a;  VERIFY (b == -2);
  b = d128 / a; VERIFY (b == 5);
}

int
main ()
{
  binary_add_32 ();
  binary_subtract_32 ();
  binary_multiply_32 ();
  binary_divide_32 ();

  binary_add_64 ();
  binary_subtract_64 ();
  binary_multiply_64 ();
  binary_divide_64 ();

  binary_add_128 ();
  binary_subtract_128 ();
  binary_multiply_128 ();
  binary_divide_128 ();
}
