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

// ISO/IEC TR 24733  3.2.7  Unary arithmetic operators.

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

decimal32 a32 (20), b32 (-20);
decimal64 a64 (124), b64 (-124);
decimal128 a128 (5001), b128 (-5001);

void
unary_plus_32 (void)
{
  decimal32 a;

  a = +a32; VERIFY (a == a32);
  a = +b32; VERIFY (a == b32);
}

void
unary_minus_32 (void)
{
  decimal32 a;

  a = -a32; VERIFY (a == b32);
  a = -b32; VERIFY (a == a32);
}

void
unary_plus_64 (void)
{
  decimal64 a;

  a = +a64; VERIFY (a == a64);
  a = +b64; VERIFY (a == b64);
}

void
unary_minus_64 (void)
{
  decimal64 a;

  a = -a64; VERIFY (a == b64);
  a = -b64; VERIFY (a == a64);
}

void
unary_plus_128 (void)
{
  decimal128 a;

  a = +a128; VERIFY (a == a128);
  a = +b128; VERIFY (a == b128);
}

void
unary_minus_128 (void)
{
  decimal128 a;

  a = -a128; VERIFY (a == b128);
  a = -b128; VERIFY (a == a128);
}

int main ()
{
  unary_plus_32 ();
  unary_minus_32 ();
  unary_plus_64 ();
  unary_minus_64 ();
  unary_plus_128 ();
  unary_minus_128 ();
}
