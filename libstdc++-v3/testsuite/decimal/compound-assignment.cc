// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.2.6  Compound assignment (decimal32).
// ISO/IEC TR 24733  3.2.3.6  Compound assignment (decimal64).
// ISO/IEC TR 24733  3.2.4.6  Compound assignment (decimal128).

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

decimal32 d32 (5);
decimal64 d64 (-10);
decimal128 d128 (25);
int si = -2;
unsigned int ui = 5;
long sl = -10;
unsigned long ul = 20;
long long sll = -25;
unsigned long long ull = 50;

void
compound_assignment_add_32 (void)
{
  decimal32 a (1000), b;

  b = a; b += d32;  VERIFY (b == 1005);
  b = a; b += d64;  VERIFY (b == 990);
  b = a; b += d128; VERIFY (b == 1025);
  b = a; b += si;   VERIFY (b == 998);
  b = a; b += ui;   VERIFY (b == 1005);
  b = a; b += sl;   VERIFY (b == 990);
  b = a; b += ul;   VERIFY (b == 1020);
  b = a; b += sll;  VERIFY (b == 975);
  b = a; b += ull;  VERIFY (b == 1050);
}

void
compound_assignment_subtract_32 (void)
{
  decimal32 a (1000), b;

  b = a; b -= d32;  VERIFY (b == 995);
  b = a; b -= d64;  VERIFY (b == 1010);
  b = a; b -= d128; VERIFY (b == 975);
  b = a; b -= si;   VERIFY (b == 1002);
  b = a; b -= ui;   VERIFY (b == 995);
  b = a; b -= sl;   VERIFY (b == 1010);
  b = a; b -= ul;   VERIFY (b == 980);
  b = a; b -= sll;  VERIFY (b == 1025);
  b = a; b -= ull;  VERIFY (b == 950);
}

void
compound_assignment_multiply_32 (void)
{
  decimal32 a (1000), b;

  b = a; b *= d32;  VERIFY (b == 5000);
  b = a; b *= d64;  VERIFY (b == -10000);
  b = a; b *= d128; VERIFY (b == 25000);
  b = a; b *= si;   VERIFY (b == -2000);
  b = a; b *= ui;   VERIFY (b == 5000);
  b = a; b *= sl;   VERIFY (b == -10000);
  b = a; b *= ul;   VERIFY (b == 20000);
  b = a; b *= sll;  VERIFY (b == -25000);
  b = a; b *= ull;  VERIFY (b == 50000);
}

void
compound_assignment_divide_32 (void)
{
  decimal32 a (1000), b;

  b = a; b /= d32;  VERIFY (b == 200);
  b = a; b /= d64;  VERIFY (b == -100);
  b = a; b /= d128; VERIFY (b == 40);
  b = a; b /= si;   VERIFY (b == -500);
  b = a; b /= ui;   VERIFY (b == 200);
  b = a; b /= sl;   VERIFY (b == -100);
  b = a; b /= ul;   VERIFY (b == 50);
  b = a; b /= sll;  VERIFY (b == -40);
  b = a; b /= ull;  VERIFY (b == 20);
}

void
compound_assignment_add_64 (void)
{
  decimal64 a (1000), b;

  b = a; b += d32;  VERIFY (b == 1005);
  b = a; b += d64;  VERIFY (b == 990);
  b = a; b += d128; VERIFY (b == 1025);
  b = a; b += si;   VERIFY (b == 998);
  b = a; b += ui;   VERIFY (b == 1005);
  b = a; b += sl;   VERIFY (b == 990);
  b = a; b += ul;   VERIFY (b == 1020);
  b = a; b += sll;  VERIFY (b == 975);
  b = a; b += ull;  VERIFY (b == 1050);
}

void
compound_assignment_subtract_64 (void)
{
  decimal64 a (1000), b;

  b = a; b -= d32;  VERIFY (b == 995);
  b = a; b -= d64;  VERIFY (b == 1010);
  b = a; b -= d128; VERIFY (b == 975);
  b = a; b -= si;   VERIFY (b == 1002);
  b = a; b -= ui;   VERIFY (b == 995);
  b = a; b -= sl;   VERIFY (b == 1010);
  b = a; b -= ul;   VERIFY (b == 980);
  b = a; b -= sll;  VERIFY (b == 1025);
  b = a; b -= ull;  VERIFY (b == 950);
}

void
compound_assignment_multiply_64 (void)
{
  decimal64 a (1000), b;

  b = a; b *= d32;  VERIFY (b == 5000);
  b = a; b *= d64;  VERIFY (b == -10000);
  b = a; b *= d128; VERIFY (b == 25000);
  b = a; b *= si;   VERIFY (b == -2000);
  b = a; b *= ui;   VERIFY (b == 5000);
  b = a; b *= sl;   VERIFY (b == -10000);
  b = a; b *= ul;   VERIFY (b == 20000);
  b = a; b *= sll;  VERIFY (b == -25000);
  b = a; b *= ull;  VERIFY (b == 50000);
}

void
compound_assignment_divide_64 (void)
{
  decimal64 a (1000), b;

  b = a; b /= d32;  VERIFY (b == 200);
  b = a; b /= d64;  VERIFY (b == -100);
  b = a; b /= d128; VERIFY (b == 40);
  b = a; b /= si;   VERIFY (b == -500);
  b = a; b /= ui;   VERIFY (b == 200);
  b = a; b /= sl;   VERIFY (b == -100);
  b = a; b /= ul;   VERIFY (b == 50);
  b = a; b /= sll;  VERIFY (b == -40);
  b = a; b /= ull;  VERIFY (b == 20);
}

void
compound_assignment_add_128 (void)
{
  decimal128 a (1000), b;

  b = a; b += d32;  VERIFY (b == 1005);
  b = a; b += d64;  VERIFY (b == 990);
  b = a; b += d128; VERIFY (b == 1025);
  b = a; b += si;   VERIFY (b == 998);
  b = a; b += ui;   VERIFY (b == 1005);
  b = a; b += sl;   VERIFY (b == 990);
  b = a; b += ul;   VERIFY (b == 1020);
  b = a; b += sll;  VERIFY (b == 975);
  b = a; b += ull;  VERIFY (b == 1050);
}

void
compound_assignment_subtract_128 (void)
{
  decimal128 a (1000), b;

  b = a; b -= d32;  VERIFY (b == 995);
  b = a; b -= d64;  VERIFY (b == 1010);
  b = a; b -= d128; VERIFY (b == 975);
  b = a; b -= si;   VERIFY (b == 1002);
  b = a; b -= ui;   VERIFY (b == 995);
  b = a; b -= sl;   VERIFY (b == 1010);
  b = a; b -= ul;   VERIFY (b == 980);
  b = a; b -= sll;  VERIFY (b == 1025);
  b = a; b -= ull;  VERIFY (b == 950);
}

void
compound_assignment_multiply_128 (void)
{
  decimal128 a (1000), b;

  b = a; b *= d32;  VERIFY (b == 5000);
  b = a; b *= d64;  VERIFY (b == -10000);
  b = a; b *= d128; VERIFY (b == 25000);
  b = a; b *= si;   VERIFY (b == -2000);
  b = a; b *= ui;   VERIFY (b == 5000);
  b = a; b *= sl;   VERIFY (b == -10000);
  b = a; b *= ul;   VERIFY (b == 20000);
  b = a; b *= sll;  VERIFY (b == -25000);
  b = a; b *= ull;  VERIFY (b == 50000);
}

void
compound_assignment_divide_128 (void)
{
  decimal128 a (1000), b;

  b = a; b /= d32;  VERIFY (b == 200);
  b = a; b /= d64;  VERIFY (b == -100);
  b = a; b /= d128; VERIFY (b == 40);
  b = a; b /= si;   VERIFY (b == -500);
  b = a; b /= ui;   VERIFY (b == 200);
  b = a; b /= sl;   VERIFY (b == -100);
  b = a; b /= ul;   VERIFY (b == 50);
  b = a; b /= sll;  VERIFY (b == -40);
  b = a; b /= ull;  VERIFY (b == 20);
}

int
main ()
{
  compound_assignment_add_32 ();
  compound_assignment_subtract_32 ();
  compound_assignment_multiply_32 ();
  compound_assignment_divide_32 ();

  compound_assignment_add_64 ();
  compound_assignment_subtract_64 ();
  compound_assignment_multiply_64 ();
  compound_assignment_divide_64 ();

  compound_assignment_add_128 ();
  compound_assignment_subtract_128 ();
  compound_assignment_multiply_128 ();
  compound_assignment_divide_128 ();
}
