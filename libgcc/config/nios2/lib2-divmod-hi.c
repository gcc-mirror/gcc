/* Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Altera and Mentor Graphics, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "lib2-nios2.h"

/* 16-bit HI divide and modulo as used in Nios II.  */

static UHItype
udivmodhi4 (UHItype num, UHItype den, word_type modwanted)
{
  UHItype bit = 1;
  UHItype res = 0;

  while (den < num && bit && !(den & (1L<<15)))
    {
      den <<=1;
      bit <<=1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res |= bit;
	}
      bit >>=1;
      den >>=1;
    }
  if (modwanted)
    return num;
  return res;
}


HItype
__divhi3 (HItype a, HItype b)
{
  word_type neg = 0;
  HItype res;

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

  res = udivmodhi4 (a, b, 0);

  if (neg)
    res = -res;

  return res;
}


HItype
__modhi3 (HItype a, HItype b)
{
  word_type neg = 0;
  HItype res;

  if (a < 0)
    {
      a = -a;
      neg = 1;
    }

  if (b < 0)
    b = -b;

  res = udivmodhi4 (a, b, 1);

  if (neg)
    res = -res;

  return res;
}


UHItype
__udivhi3 (UHItype a, UHItype b)
{
  return udivmodhi4 (a, b, 0);
}


UHItype
__umodhi3 (UHItype a, UHItype b)
{
  return udivmodhi4 (a, b, 1);
}

