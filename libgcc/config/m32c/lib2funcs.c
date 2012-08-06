/* libgcc routines for R8C/M16C/M32C
   Copyright (C) 2005, 2009
   Free Software Foundation, Inc.
   Contributed by Red Hat.

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

typedef          int  sint32_type   __attribute__ ((mode (SI)));
typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));
typedef int           word_type     __attribute__ ((mode (__word__)));

uint32_type udivmodsi4 (uint32_type, uint32_type, word_type);
sint32_type __divsi3   (sint32_type, sint32_type);
sint32_type __modsi3   (sint32_type, sint32_type);

uint32_type
udivmodsi4 (uint32_type num, uint32_type den, word_type modwanted)
{
  uint32_type bit = 1;
  uint32_type res = 0;

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

sint32_type
__divsi3 (sint32_type a, sint32_type b)
{
  word_type neg = 0;
  sint32_type res;

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

sint32_type
__modsi3 (sint32_type a, sint32_type b)
{
  word_type neg = 0;
  sint32_type res;

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

/* See the comment by the definition of LIBGCC2_UNITS_PER_WORD in
   m32c.h for why we are creating extra versions of some of the
   functions defined in libgcc2.c.  */

#define LIBGCC2_UNITS_PER_WORD 2

#define L_clzsi2
#define L_ctzsi2
#define L_ffssi2
#define L_paritysi2
#define L_popcountsi2

#include "libgcc2.c"

uint32_type
__udivsi3 (uint32_type a, uint32_type b)
{
  return udivmodsi4 (a, b, 0);
}

uint32_type
__umoddi3 (uint32_type a, uint32_type b)
{
  return udivmodsi4 (a, b, 1);
}
