/* libgcc routines for ARC64
   Copyright (C) 2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


typedef          int  sint64_t   __attribute__ ((mode (DI)));
typedef unsigned int  uint64_t   __attribute__ ((mode (DI)));
typedef int           word_t     __attribute__ ((mode (__word__)));


sint64_t __muldi3 (sint64_t, sint64_t);
sint64_t __divdi3 (sint64_t, sint64_t);
sint64_t __moddi3 (sint64_t, sint64_t);

sint64_t
__muldi3 (sint64_t a, sint64_t b)
{
  sint64_t res = 0;
  uint64_t cnt = a;

  while (cnt)
    {
      if (cnt & 1)
        res += b;
      b <<= 1;
      cnt >>= 1;
    }
  return res;
}

/* Unsigned integer division/modulus.  */

static inline __attribute__ ((__always_inline__))
uint64_t
udivmoddi4 (uint64_t num, uint64_t den, word_t modwanted)
{
  uint64_t bit = 1;
  uint64_t res = 0;

  while (den < num && bit && !(den & (1LL << 63)))
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

sint64_t
__divdi3 (sint64_t a, sint64_t b)
{
  word_t neg = 0;
  sint64_t res;

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

  res = udivmoddi4 (a, b, 0);

  if (neg)
    res = -res;

  return res;
}

sint64_t
__moddi3 (sint64_t a, sint64_t b)
{
  word_t neg = 0;
  sint64_t res;

  if (a < 0)
    {
      a = -a;
      neg = 1;
    }

  if (b < 0)
    b = -b;

  res = udivmoddi4 (a, b, 1);

  if (neg)
    res = -res;

  return res;
}

uint64_t
__udivdi3 (uint64_t a, uint64_t b)
{
  return udivmoddi4 (a, b, 0);
}

uint64_t
__umoddi3 (uint64_t a, uint64_t b)
{
  return udivmoddi4 (a, b, 1);
}

/* We need 32bit version for some of the functions defined in
   libgcc2.c.  */
#define LIBGCC2_UNITS_PER_WORD 4

#define L_clzsi2
#define L_ctzsi2
#define L_ffssi2
#define L_paritysi2
#define L_popcountsi2

#include "libgcc2.c"
