/* Shift functions for the GCC support library for the Renesas RL78 processors.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef          int  sint32_type   __attribute__ ((mode (SI)));
typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));
typedef          int  sint16_type   __attribute__ ((mode (HI)));
typedef unsigned int  uint16_type   __attribute__ ((mode (HI)));

uint32_type __ashlsi3 (uint32_type in, char bit);
sint32_type __ashrsi3 (sint32_type in, char bit);
int __clrsbhi2 (sint16_type x);
extern int __clrsbsi2 (sint32_type x);

typedef struct
{
  union
  {
    uint32_type u;
    uint16_type h[2];
  } u;
} dd;

uint32_type
__ashlsi3 (uint32_type in, char bit)
{
  uint16_type h, l;
  dd d;

  if (bit > 32)
    return 0;
  if (bit < 0)
    return in;

  d.u.u = in;
  h = d.u.h[1];
  l = d.u.h[0];

  if (bit > 15)
    {
      h = l;
      l = 0;
      bit -= 16;
    }

  while (bit)
    {
      h = (h << 1) | (l >> 15);
      l <<= 1;
      bit --;
    }

  d.u.h[1] = h;
  d.u.h[0] = l;
  return d.u.u;
}

sint32_type
__ashrsi3 (sint32_type in, char bit)
{
  sint16_type h;
  uint16_type l;
  dd d;

  if (bit > 32)
    return 0;
  if (bit < 0)
    return in;

  d.u.u = in;
  h = d.u.h[1];
  l = d.u.h[0];

  while (bit)
    {
      l = (h << 15) | (l >> 1);
      h >>= 1;
      bit --;
    }

  d.u.h[1] = h;
  d.u.h[0] = l;
  return d.u.u;
}

int
__clrsbhi2 (sint16_type x)
{
  if (x == 0)
    return 15;
  return __clrsbsi2 ((sint32_type) x) - 16;
}
