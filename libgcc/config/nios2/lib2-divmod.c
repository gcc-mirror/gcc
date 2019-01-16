/* Copyright (C) 2012-2019 Free Software Foundation, Inc.
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

/* 32-bit SI divide and modulo as used in Nios II.  */

static USItype
udivmodsi4 (USItype num, USItype den, word_type modwanted)
{
  USItype bit = 1;
  USItype res = 0;

  while (den < num && bit && !(den & (1L<<31)))
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


SItype
__udivsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 0);
}


SItype
__umodsi3 (SItype a, SItype b)
{
  return udivmodsi4 (a, b, 1);
}

