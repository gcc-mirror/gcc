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

// ISO/IEC TR 24733  3.2.2.5  Increment and decrement operators (decimal32).
// ISO/IEC TR 24733  3.2.3.5  Increment and decrement operators (decimal64).
// ISO/IEC TR 24733  3.2.4.5  Increment and decrement operators (decimal128).

#include <decimal/decimal>
#include <testsuite_hooks.h>

// Use extension to replace implicit long long conversion with function call.
#define LONGLONG(X) decimal_to_long_long(X)

using namespace std::decimal;

void
incdec32 (void)
{
  int ival;
  decimal32 a(11), b, c;

  // Verify that we get the expected value of b after assignment.
  b = a;
  ival = LONGLONG (b); VERIFY (ival == 11);

  // Check that the increment and decrement operators change the value
  // of the original class.
  b = a;
  ++b;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  b++;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  --b;
  ival = LONGLONG (b); VERIFY (ival == 10);

  b = a;
  b--;
  ival = LONGLONG (b); VERIFY (ival == 10);

  // Check that the increment and decrement operators return the
  // correct value.
  b = a;
  c = ++b;
  ival = LONGLONG (c); VERIFY (ival == 12);

  b = a;
  c = b++;
  ival = LONGLONG (c); VERIFY (ival == 11);

  b = a;
  c = --b;
  ival = LONGLONG (c); VERIFY (ival == 10);

  b = a;
  c = b--;
  ival = LONGLONG (c); VERIFY (ival == 11);
}

void
incdec64 (void)
{
  int ival;
  decimal64 a(11), b, c;

  // Verify that we get the expected value of b after assignment.
  b = a;
  ival = LONGLONG (b); VERIFY (ival == 11);

  // Check that the increment and decrement operators change the value
  // of the original class.
  b = a;
  ++b;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  b++;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  --b;
  ival = LONGLONG (b); VERIFY (ival == 10);

  b = a;
  b--;
  ival = LONGLONG (b); VERIFY (ival == 10);

  // Check that the increment and decrement operators return the
  // correct value.
  b = a;
  c = ++b;
  ival = LONGLONG (c); VERIFY (ival == 12);

  b = a;
  c = b++;
  ival = LONGLONG (c); VERIFY (ival == 11);

  b = a;
  c = --b;
  ival = LONGLONG (c); VERIFY (ival == 10);

  b = a;
  c = b--;
  ival = LONGLONG (c); VERIFY (ival == 11);
}

void
incdec128 (void)
{
  int ival;
  decimal128 a(11), b, c;

  // Verify that we get the expected value of b after assignment.
  b = a;
  ival = LONGLONG (b); VERIFY (ival == 11);

  // Check that the increment and decrement operators change the value
  // of the original class.
  b = a;
  ++b;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  b++;
  ival = LONGLONG (b); VERIFY (ival == 12);

  b = a;
  --b;
  ival = LONGLONG (b); VERIFY (ival == 10);

  b = a;
  b--;
  ival = LONGLONG (b); VERIFY (ival == 10);

  // Check that the increment and decrement operators return the
  // correct value.
  b = a;
  c = ++b;
  ival = LONGLONG (c); VERIFY (ival == 12);

  b = a;
  c = b++;
  ival = LONGLONG (c); VERIFY (ival == 11);

  b = a;
  c = --b;
  ival = LONGLONG (c); VERIFY (ival == 10);

  b = a;
  c = b--;
  ival = LONGLONG (c); VERIFY (ival == 11);
}

int
main ()
{
  incdec32 ();
  incdec64 ();
  incdec128 ();
}
