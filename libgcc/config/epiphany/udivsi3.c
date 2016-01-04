/* Generic unsigned 32 bit division implementation.
   Copyright (C) 2009-2016 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

This file is part of GCC.

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

typedef union { unsigned int i; float f; } fu;

unsigned int __udivsi3 (unsigned int a, unsigned int b);

unsigned int
__udivsi3 (unsigned int a, unsigned int b)
{
  unsigned int d, t, s0, s1, s2, r0, r1;
  fu u0, u1, u2,  u1b, u2b;

  if (b > a)
    return 0;

  /* Compute difference in number of bits in S0.  */
  u0.i = 0x40000000;
  u1b.i = u2b.i = u0.i;
  u1.i = a;
  u2.i = b;
  u1.i = a | u0.i;
  t = 0x4b800000 | ((a >> 23) & 0xffff);
  if (a >> 23)
    {
      u1.i = t;
      u1b.i = 0x4b800000;
    }
  u2.i = b | u0.i;
  t = 0x4b800000 | ((b >> 23) & 0xffff);
  if (b >> 23)
    {
      u2.i = t;
      u2b.i = 0x4b800000;
    }
  u1.f = u1.f - u1b.f;
  u2.f = u2.f - u2b.f;
  s1 = u1.i >> 23;
  s2 = u2.i >> 23;
  s0 = s1 - s2;

  b <<= s0;
  d = b - 1;

  r0 = 1 << s0;
  r1 = 0;
  t = a - b;
  if (t <= a)
    {
      a = t;
      r1 = r0;
    }

#define STEP(n) case n: a += a; t = a - d; if (t <= a) a = t;
  switch (s0)
    {
    STEP (31)
    STEP (30)
    STEP (29)
    STEP (28)
    STEP (27)
    STEP (26)
    STEP (25)
    STEP (24)
    STEP (23)
    STEP (22)
    STEP (21)
    STEP (20)
    STEP (19)
    STEP (18)
    STEP (17)
    STEP (16)
    STEP (15)
    STEP (14)
    STEP (13)
    STEP (12)
    STEP (11)
    STEP (10)
    STEP (9)
    STEP (8)
    STEP (7)
    STEP (6)
    STEP (5)
    STEP (4)
    STEP (3)
    STEP (2)
    STEP (1)
    case 0: ;
    }
  r0 = r1 | (r0-1 & a);
  return r0;
}
