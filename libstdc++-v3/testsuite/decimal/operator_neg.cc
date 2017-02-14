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

// { dg-do compile }
// { dg-require-effective-target dfp }

// Test that C++ binary operators that are restricted to integer operands
// do not accept decimal float operands.

#include <decimal/decimal>

using namespace std::decimal;

decimal32 a32, b32, c32;
decimal64 a64, b64, c64;
decimal128 a128, b128, c128;

void
modulus (void)
{
  a32 = b32 % c32;	// { dg-error "error" } 
  a64 = b64 % c64;	// { dg-error "error" } 
  a128 = b128 % c128;	// { dg-error "error" } 
  a128 = b32 % c128;	// { dg-error "error" } 
  a128 = b64 % c128;	// { dg-error "error" } 
  a32 = 100 % c32;	// { dg-error "error" } 
  a64 = 10 % c64;	// { dg-error "error" } 
  a128 = 1000 % c128;	// { dg-error "error" } 
  a32 = b32 % 7;	// { dg-error "error" } 
  a64 = b64 % 5;	// { dg-error "error" } 
  a128 = b128 % 3;	// { dg-error "error" } 
}

void
bitwise_right_shift (void)
{
  a32 = b32 >> c32;	// { dg-error "error" }
  a64 = b64 >> c64;	// { dg-error "error" }
  a128 = b128 >> c128;	// { dg-error "error" }
  a128 = b32 >> c128;	// { dg-error "error" }
  a128 = b64 >> c128;	// { dg-error "error" }
  a32 = 100 >> c32;	// { dg-error "error" }
  a64 = 10 >> c64;	// { dg-error "error" }
  a128 = 1000 >> c128;	// { dg-error "error" }
  a32 = b32 >> 7;	// { dg-error "error" }
  a64 = b64 >> 5;	// { dg-error "error" }
  a128 = b128 >> 3;	// { dg-error "error" }
}

void
bitwise_left_shift (void)
{
  a32 = b32 << c32;	// { dg-error "error" } 
  a64 = b64 << c64;	// { dg-error "error" } 
  a128 = b128 << c128;	// { dg-error "error" } 
  a128 = b32 << c128;	// { dg-error "error" } 
  a128 = b64 << c128;	// { dg-error "error" } 
  a32 = 100 << c32;	// { dg-error "error" } 
  a64 = 10 << c64;	// { dg-error "error" } 
  a128 = 1000 << c128;	// { dg-error "error" } 
  a32 = b32 << 7;	// { dg-error "error" } 
  a64 = b64 << 5;	// { dg-error "error" } 
  a128 = b128 << 3;	// { dg-error "error" } 
}

void
bitwise_exclusive_or (void)
{
  a32 = b32 ^ c32;	// { dg-error "error" } 
  a64 = b64 ^ c64;	// { dg-error "error" } 
  a128 = b128 ^ c128;	// { dg-error "error" } 
  a128 = b32 ^ c128;	// { dg-error "error" } 
  a128 = b64 ^ c128;	// { dg-error "error" } 
  a32 = 100 ^ c32;	// { dg-error "error" } 
  a64 = 10 ^ c64;	// { dg-error "error" } 
  a128 = 1000 ^ c128;	// { dg-error "error" } 
  a32 = b32 ^ 7;	// { dg-error "error" } 
  a64 = b64 ^ 5;	// { dg-error "error" } 
  a128 = b128 ^ 3;	// { dg-error "error" } 
}

void
bitwise_inclusive_or (void)
{
  a32 = b32 | c32;	// { dg-error "error" } 
  a64 = b64 | c64;	// { dg-error "error" } 
  a128 = b128 | c128;	// { dg-error "error" } 
  a128 = b32 | c128;	// { dg-error "error" } 
  a128 = b64 | c128;	// { dg-error "error" } 
  a32 = 100 | c32;	// { dg-error "error" } 
  a64 = 10 | c64;	// { dg-error "error" } 
  a128 = 1000 | c128;	// { dg-error "error" } 
  a32 = b32 | 7;	// { dg-error "error" } 
  a64 = b64 | 5;	// { dg-error "error" } 
  a128 = b128 | 3;	// { dg-error "error" } 
}

void
logical_and (void)
{
  a32 = b32 && c32;	// { dg-error "error" } 
  a64 = b64 && c64;	// { dg-error "error" } 
  a128 = b128 && c128;	// { dg-error "error" } 
  a128 = b32 && c128;	// { dg-error "error" } 
  a128 = b64 && c128;	// { dg-error "error" } 
  a32 = 100 && c32;	// { dg-error "error" } 
  a64 = 10 && c64;	// { dg-error "error" } 
  a128 = 1000 && c128;	// { dg-error "error" } 
  a32 = b32 && 7;	// { dg-error "error" } 
  a64 = b64 && 5;	// { dg-error "error" } 
  a128 = b128 && 3;	// { dg-error "error" } 
}

void
logical_or (void)
{
  a32 = b32 || c32;	// { dg-error "error" } 
  a64 = b64 || c64;	// { dg-error "error" } 
  a128 = b128 || c128;	// { dg-error "error" } 
  a128 = b32 || c128;	// { dg-error "error" } 
  a128 = b64 || c128;	// { dg-error "error" } 
  a32 = 100 || c32;	// { dg-error "error" } 
  a64 = 10 || c64;	// { dg-error "error" } 
  a128 = 1000 || c128;	// { dg-error "error" } 
  a32 = b32 || 7;	// { dg-error "error" } 
  a64 = b64 || 5;	// { dg-error "error" } 
  a128 = b128 || 3;	// { dg-error "error" } 
}

void
bitwise_complement (void)
{
  a32 = ~b32;		// { dg-error "error" } 
  a64 = ~b64;		// { dg-error "error" } 
  a128 = ~b128;		// { dg-error "error" } 
}

void
logical_not (void)
{
  a32 = !b32;		// { dg-error "error" } 
  a64 = !b64;		// { dg-error "error" } 
  a128 = !b128;		// { dg-error "error" } 
}

