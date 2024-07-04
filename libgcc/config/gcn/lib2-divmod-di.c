/* Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Mentor Graphics, Inc.

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

#include "lib2-gcn.h"

/* 64-bit SI divide and modulo as used in gcn.  */

union pack {
  UTItype ti;
  struct {DItype quot, rem;} pair;
};
union upack {
  UTItype ti;
  struct {UDItype quot, rem;} pair;
};

UTItype
__udivmoddi4 (UDItype num, UDItype den)
{
  UDItype bit = 1;
  union upack res = {0};

  while (den < num && bit && !(den & (1L<<63)))
    {
      den <<=1;
      bit <<=1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res.pair.quot |= bit;
	}
      bit >>=1;
      den >>=1;
    }
  res.pair.rem = num;
  return res.ti;
}

UTItype
__divmoddi4 (DItype a, DItype b)
{
  word_type nega = 0, negb = 0;
  union pack res;

  if (a < 0)
    {
      a = -a;
      nega = 1;
    }

  if (b < 0)
    {
      b = -b;
      negb = 1;
    }

  res.ti = __udivmoddi4 (a, b);

  if (nega)
    res.pair.rem = -res.pair.rem;
  if (nega ^ negb)
    res.pair.quot = -res.pair.quot;

  return res.ti;
}


DItype
__divdi3 (DItype a, DItype b)
{
  union pack u;
  u.ti = __divmoddi4 (a, b);
  return u.pair.quot;
}

DItype
__moddi3 (DItype a, DItype b)
{
  union pack u;
  u.ti = __divmoddi4 (a, b);
  return u.pair.rem;
}


UDItype
__udivdi3 (UDItype a, UDItype b)
{
  union pack u;
  u.ti = __udivmoddi4 (a, b);
  return u.pair.quot;
}

UDItype
__umoddi3 (UDItype a, UDItype b)
{
  union pack u;
 u.ti = __udivmoddi4 (a, b);
 return u.pair.rem;
}

