// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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
// { dg-options "-Wno-pedantic" }

// ISO/IEC TR 24733  3.2.5  Initialization from coefficient and exponent.

#include <decimal/decimal>
#include <cstring>
#include <testsuite_hooks.h>

#define PASTE2(A,B) A ## B
#define PASTE(A,B) PASTE2(A,B)

#define TESTVAL_NEG(COEFF,ESIGN,EXP,SUF,NUM,SIZE)			\
  x = PASTE(PASTE(PASTE(PASTE(PASTE(COEFF,.),E),ESIGN),EXP),SUF);	\
  sll = PASTE(COEFF,LL);						\
  i = ESIGN EXP;							\
  a = PASTE(make_decimal,32) (sll, i);					\
  b = PASTE(make_decimal,32) (PASTE(COEFF,LL), ESIGN EXP);	\
  VERIFY ((std::memcmp ((void *)&x, (void *)&a, SIZE) == 0)	\
          && (std::memcmp ((void *)&x, (void *)&b,SIZE) == 0));

#define TESTVAL_NONNEG(COEFF,ESIGN,EXP,SUF,NUM,SIZE)			\
  x = PASTE(PASTE(PASTE(PASTE(PASTE(COEFF,.),E),ESIGN),EXP),SUF);	\
  sll = PASTE(COEFF,LL);						\
  ull = PASTE(COEFF,ULL);						\
  i = ESIGN EXP;							\
  a = PASTE(make_decimal,32) (sll, i);					\
  b = PASTE(make_decimal,32) (PASTE(COEFF,LL), ESIGN EXP);		\
  c = PASTE(make_decimal,32) (ull, i);					\
  d = PASTE(make_decimal,32) (PASTE(COEFF,ULL), ESIGN EXP);		\
  VERIFY ((std::memcmp ((void *)&x, (void *)&a, SIZE) == 0)	\
          && (std::memcmp ((void *)&x, (void *)&b,SIZE) == 0)	\
          && (std::memcmp ((void *)&x, (void *)&c,SIZE) == 0)	\
          && (std::memcmp ((void *)&x, (void *)&d,SIZE) == 0));

using namespace std::decimal;

void
make_decimal_32 (void)
{
  decimal32 a, b, c, d;
  float x __attribute__((mode(SD)));
  int i;
  unsigned long sz = sizeof (decimal32);
  volatile long long sll;
  volatile unsigned long long ull;

  TESTVAL_NONNEG (0, +, 0, DF, 32, sz); 
  TESTVAL_NONNEG (5, +, 1, DF, 32, sz);
  TESTVAL_NONNEG (50, +, 0, DF, 32, sz);
  TESTVAL_NONNEG (500, +, 0, DF, 32, sz);
  TESTVAL_NEG (-25, -, 3, DF, 32, sz)
  TESTVAL_NEG (-500, +, 0, DF, 32, sz);
  TESTVAL_NONNEG (999999, +, 91, DF, 32, sz);
  TESTVAL_NONNEG (1, -, 9, DF, 32, sz);
  TESTVAL_NONNEG (1, -, 90, DF, 32, sz);
  TESTVAL_NONNEG (1, -, 95, DF, 32, sz);
  TESTVAL_NONNEG (1, -, 101, DF, 32, sz);
  TESTVAL_NEG (-1, -, 101, DF, 32, sz);
}

void
make_decimal_64 (void)
{
  decimal64 a, b, c, d;
  float x __attribute__((mode(DD)));
  int i;
  unsigned long sz = sizeof (decimal64);
  volatile long long sll;
  volatile unsigned long long ull;

  TESTVAL_NONNEG (0, +, 0, DF, 64, sz); 
  TESTVAL_NONNEG (5, +, 1, DF, 64, sz);
  TESTVAL_NONNEG (50, +, 0, DF, 64, sz);
  TESTVAL_NONNEG (500, +, 0, DF, 64, sz);
  TESTVAL_NEG (-25, -, 3, DF, 64, sz)
  TESTVAL_NEG (-500, +, 0, DF, 64, sz);
  TESTVAL_NONNEG (999999, +, 91, DF, 64, sz);
  TESTVAL_NONNEG (1, -, 9, DF, 64, sz);
  TESTVAL_NONNEG (1, -, 90, DF, 64, sz);
  TESTVAL_NONNEG (1, -, 95, DF, 64, sz);
  TESTVAL_NONNEG (1, -, 101, DF, 64, sz);
  TESTVAL_NEG (-1, -, 101, DF, 64, sz);
}

void
make_decimal_128 (void)
{
  decimal128 a, b, c, d;
  float x __attribute__((mode(TD)));
  int i;
  unsigned long sz = sizeof (decimal128);
  volatile long long sll;
  volatile unsigned long long ull;

  TESTVAL_NONNEG (0, +, 0, DF, 128, sz); 
  TESTVAL_NONNEG (5, +, 1, DF, 128, sz);
  TESTVAL_NONNEG (50, +, 0, DF, 128, sz);
  TESTVAL_NONNEG (500, +, 0, DF, 128, sz);
  TESTVAL_NEG (-25, -, 3, DF, 128, sz)
  TESTVAL_NEG (-500, +, 0, DF, 128, sz);
  TESTVAL_NONNEG (999999, +, 91, DF, 128, sz);
  TESTVAL_NONNEG (1, -, 9, DF, 128, sz);
  TESTVAL_NONNEG (1, -, 90, DF, 128, sz);
  TESTVAL_NONNEG (1, -, 95, DF, 128, sz);
  TESTVAL_NONNEG (1, -, 101, DF, 128, sz);
  TESTVAL_NEG (-1, -, 101, DF, 128, sz);
}

int
main ()
{
  make_decimal_32 ();
  make_decimal_64 ();
  make_decimal_128 ();
}
