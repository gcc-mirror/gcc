// Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

// ISO/IEC TR 24733  3.2.2.1  Construct/copy/destroy (decimal32).
// ISO/IEC TR 24733  3.2.3.1  Construct/copy/destroy (decimal64).
// ISO/IEC TR 24733  3.2.4.1  Construct/copy/destroy (decimal128).

// Test the default constructor.

#include <decimal/decimal>
#include <cstring>
#include <testsuite_hooks.h>

using namespace std::decimal;

void
ctor_32 (void)
{
  bool test __attribute__((unused)) = true;
  decimal32 a;
  float b __attribute__((mode(SD))) = 0.e-101DF;

  VERIFY (std::memcmp (&a, &b, 4) == 0);
}

void
ctor_64 (void)
{
  bool test __attribute__((unused)) = true;
  decimal64 a;
  float b __attribute__((mode(DD))) = 0.e-398DD;

  VERIFY (std::memcmp (&a, &b, 8) == 0);
}

void
ctor_128 (void)
{
  bool test __attribute__((unused)) = true;
  decimal128 a;
  float b __attribute__((mode(TD))) = 0.e-6176DL;

  VERIFY (std::memcmp (&a, &b, 16) == 0);
}

int
main ()
{
  ctor_32 ();
  ctor_64 ();
  ctor_128 ();
}
