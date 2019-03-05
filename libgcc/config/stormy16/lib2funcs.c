/* This file contains 16-bit versions of some of the functions found in
   libgcc2.c.  Really libgcc ought to be moved out of the gcc directory
   and into its own top level directory, and then split up into multiple
   files.  On this glorious day maybe this code can be integrated into
   it too.  */

/* Copyright (C) 2005-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

#ifdef HAVE_GAS_HIDDEN
#define ATTRIBUTE_HIDDEN  __attribute__ ((__visibility__ ("hidden")))
#else
#define ATTRIBUTE_HIDDEN
#endif

#ifndef MIN_UNITS_PER_WORD
#define MIN_UNITS_PER_WORD UNITS_PER_WORD
#endif

#ifndef LIBGCC2_UNITS_PER_WORD
# if MIN_UNITS_PER_WORD > 4
#  define LIBGCC2_UNITS_PER_WORD 8
# elif (MIN_UNITS_PER_WORD > 2 \
        || (MIN_UNITS_PER_WORD > 1 && LONG_LONG_TYPE_SIZE > 32))
#  define LIBGCC2_UNITS_PER_WORD 4
# else
#  define LIBGCC2_UNITS_PER_WORD MIN_UNITS_PER_WORD
# endif
#endif

#define word_type Wtype

#include "libgcc2.h"
#undef int

/* These prototypes would normally live in libgcc2.h, but this can
   only happen once the code below is integrated into libgcc2.c.  */

extern USItype udivmodsi4 (USItype, USItype, word_type);
extern SItype __divsi3 (SItype, SItype);
extern SItype __modsi3 (SItype, SItype);
extern SItype __udivsi3 (SItype, SItype);
extern SItype __umodsi3 (SItype, SItype);
extern SItype __ashlsi3 (SItype, SItype);
extern SItype __ashrsi3 (SItype, SItype);
extern USItype __lshrsi3 (USItype, USItype);
extern int __popcounthi2 (UHWtype);
extern int __parityhi2 (UHWtype);
extern int __clzhi2 (UHWtype);
extern int __ctzhi2 (UHWtype);


#ifdef XSTORMY16_UDIVMODSI4
USItype
udivmodsi4 (USItype num, USItype den, word_type modwanted)
{
  USItype bit = 1;
  USItype res = 0;

  while (den < num && bit && !(den & (1L << 31)))
    {
      den <<= 1;
      bit <<= 1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res |= bit;
	}
      bit >>= 1;
      den >>= 1;
    }

  if (modwanted)
    return num;
  return res;
}
#endif

#ifdef XSTORMY16_DIVSI3
SItype
__divsi3 (SItype a, SItype b)
{
  word_type neg = 0;
  SItype res;

  if (a < 0)
    {
      a = -a;
      neg = !neg;
    }

  if (b < 0)
    {
      b = -b;
      neg = !neg;
    }

  res = udivmodsi4 (a, b, 0);

  if (neg)
    res = -res;

  return res;
}
#endif

#ifdef XSTORMY16_MODSI3
SItype
__modsi3 (SItype a, SItype b)
{
  word_type neg = 0;
  SItype res;

  if (a < 0)
    {
      a = -a;
      neg = 1;
    }

  if (b < 0)
    b = -b;

  res = udivmodsi4 (a, b, 1);

  if (neg)
    res = -res;

  return res;
}
#endif

#ifdef XSTORMY16_UDIVSI3
SItype
__udivsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 0);
}
#endif

#ifdef XSTORMY16_UMODSI3
SItype
__umodsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 1);
}
#endif

#ifdef XSTORMY16_ASHLSI3
SItype
__ashlsi3 (SItype a, SItype b)
{
  word_type i;
  
  if (b & 16)
    a <<= 16;
  if (b & 8)
    a <<= 8;
  for (i = (b & 0x7); i > 0; --i)
    a <<= 1;
  return a;
}
#endif

#ifdef XSTORMY16_ASHRSI3
SItype
__ashrsi3 (SItype a, SItype b)
{
  word_type i;
  
  if (b & 16)
    a >>= 16;
  if (b & 8)
    a >>= 8;
  for (i = (b & 0x7); i > 0; --i)
    a >>= 1;
  return a;
}
#endif

#ifdef XSTORMY16_LSHRSI3
USItype
__lshrsi3 (USItype a, USItype b)
{
  word_type i;
  
  if (b & 16)
    a >>= 16;
  if (b & 8)
    a >>= 8;
  for (i = (b & 0x7); i > 0; --i)
    a >>= 1;
  return a;
}
#endif

#ifdef XSTORMY16_POPCOUNTHI2
/* Returns the number of set bits in X.
   FIXME:  The return type really should be "unsigned int"
   but this is not how the builtin is prototyped.  */
int
__popcounthi2 (UHWtype x)
{
  int ret;

  ret = __popcount_tab [x & 0xff];
  ret += __popcount_tab [(x >> 8) & 0xff];

  return ret;
}
#endif

#ifdef XSTORMY16_PARITYHI2
/* Returns the number of set bits in X, modulo 2.
   FIXME:  The return type really should be "unsigned int"
   but this is not how the builtin is prototyped.  */

int
__parityhi2 (UHWtype x)
{
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0xf;
  return (0x6996 >> x) & 1;
}
#endif

#ifdef XSTORMY16_CLZHI2
/* Returns the number of zero-bits from the most significant bit to the
   first nonzero bit in X.  Returns 16 for X == 0.  Implemented as a
   simple for loop in order to save space by removing the need for
   the __clz_tab array.
   FIXME:  The return type really should be "unsigned int" but this is
   not how the builtin is prototyped.  */
#undef unsigned
int
__clzhi2 (UHWtype x)
{
  unsigned int i;
  unsigned int c;
  unsigned int value = x;

  for (c = 0, i = 1 << 15; i; i >>= 1, c++)
    if (i & value)
      break;
  return c;
}
#endif

#ifdef XSTORMY16_CTZHI2
/* Returns the number of trailing zero bits in X.
   FIXME:  The return type really should be "signed int" since
   ctz(0) returns -1, but this is not how the builtin is prototyped.  */

int
__ctzhi2 (UHWtype x)
{
  /* This is cunning.  It converts X into a number with only the one bit
     set, the bit that was the least significant bit in X.  From this we
     can use the count_leading_zeros to compute the number of trailing
     bits.  */
  x &= - x;

  return 15 - __builtin_clz (x);
}
#endif

#ifdef XSTORMY16_FFSHI2
/* Returns one plus the index of the least significant 1-bit of X,
   or if X is zero, returns zero.  FIXME:  The return type really
   should be "unsigned int" but this is not how the builtin is
   prototyped.  */

int
__ffshi2 (UHWtype u)
{
  UHWtype count;

  if (u == 0)
    return 0;

  return 16 - __builtin_clz (u & - u);
}
#endif

#ifdef XSTORMY16_CLRSBHI2
/* Returns the number of leading redundant sign bits in X.
   I.e. the number of bits following the most significant bit which are
   identical to it.  There are no special cases for 0 or other values.  */

int
__clrsbhi2 (HWtype x)
{
  if (x < 0)
    x = ~x;
  if (x == 0)
    return 15;
  return __builtin_clz (x) - 1;
}
#endif

#ifdef XSTORMY16_UCMPSI2
/* Performs an unsigned comparison of two 32-bit values: A and B.
   If A is less than B, then 0 is returned.  If A is greater than B,
   then 2 is returned.  Otherwise A and B are equal and 1 is returned.  */

word_type
__ucmpsi2 (USItype a, USItype b)
{
  word_type hi_a = (a >> 16);
  word_type hi_b = (b >> 16);

  if (hi_a == hi_b)
    {
      word_type low_a = (a & 0xffff);
      word_type low_b = (b & 0xffff);

      return low_a < low_b ? 0 : (low_a > low_b ? 2 : 1);
    }

  return hi_a < hi_b ? 0 : 2;
}
#endif

#ifdef XSTORMY16_CMPSI2
/* Performs an signed comparison of two 32-bit values: A and B.
   If A is less than B, then 0 is returned.  If A is greater than B,
   then 2 is returned.  Otherwise A and B are equal and 1 is returned.  */

word_type
__cmpsi2 (SItype a, SItype b)
{
  word_type hi_a = (a >> 16);
  word_type hi_b = (b >> 16);

  if (hi_a == hi_b)
    {
      word_type low_a = (a & 0xffff);
      word_type low_b = (b & 0xffff);

      return low_a < low_b ? 0 : (low_a > low_b ? 2 : 1);
    }

  return hi_a < hi_b ? 0 : 2;
}
#endif
