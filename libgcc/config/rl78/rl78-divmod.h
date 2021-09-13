/* libgcc routines for RL78
   Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

UINT_TYPE C3(udivmod,NAME_MODE,4) (UINT_TYPE, UINT_TYPE, word_type);
SINT_TYPE C3(__div,NAME_MODE,3)   (SINT_TYPE, SINT_TYPE);
SINT_TYPE C3(__mod,NAME_MODE,3)   (SINT_TYPE, SINT_TYPE);
UINT_TYPE C3(__udiv,NAME_MODE,3)  (UINT_TYPE, UINT_TYPE);
UINT_TYPE C3(__umod,NAME_MODE,3)  (UINT_TYPE, UINT_TYPE);

UINT_TYPE
C3(udivmod,NAME_MODE,4) (UINT_TYPE num, UINT_TYPE den, word_type modwanted)
{
  UINT_TYPE bit = 1;
  UINT_TYPE res = 0;

  while (den < num && bit && !(den & (1L << BITS_MINUS_1)))
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

SINT_TYPE
C3(__div,NAME_MODE,3) (SINT_TYPE a, SINT_TYPE b)
{
  word_type neg = 0;
  SINT_TYPE res;

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

  res = C3(udivmod,NAME_MODE,4) (a, b, 0);

  if (neg)
    res = -res;

  return res;
}

SINT_TYPE
C3(__mod,NAME_MODE,3) (SINT_TYPE a, SINT_TYPE b)
{
  word_type neg = 0;
  SINT_TYPE res;

  if (a < 0)
    {
      a = -a;
      neg = 1;
    }

  if (b < 0)
    b = -b;

  res = C3(udivmod,NAME_MODE,4) (a, b, 1);

  if (neg)
    res = -res;

  return res;
}

UINT_TYPE
C3(__udiv,NAME_MODE,3) (UINT_TYPE a, UINT_TYPE b)
{
  return C3(udivmod,NAME_MODE,4) (a, b, 0);
}

UINT_TYPE
C3(__umod,NAME_MODE,3) (UINT_TYPE a, UINT_TYPE b)
{
  return C3(udivmod,NAME_MODE,4) (a, b, 1);
}
