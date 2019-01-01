/* Generic unsigned 32 bit division implementation.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
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
  if ((int) b < 0)
    return 1;

  /* Assuming B is nonzero, compute S0 such that 0 <= S0,
     (B << S0+1) does not overflow,
     A < 4.01 * (B << S0), with S0 chosen as small as possible
     without taking to much time calculating.  */
#ifdef CONVERT_UNSIGNED
  u0.f = a;
  u1.f = b;
#else /* !CONVERT_UNSIGNED */
  u0.f = (int) a;
  u1.f = (int) b;
#ifdef CONCISE
  if ((int) a < 0)
    u0.i = (a >> 8) - 0x00800000 + 0x3f800000 + (31 << 23);
#else /* To use flag setting / cmove, this can be written as:  */
 {
  unsigned c = 0xff800000 - 0x4f000000;
  t = (int)a >> 8;
  if (t >= c)
    u0.i = (t - c);
 }
#endif
#endif /* !CONVERT_UNSIGNED */
  s0 = u0.i + 1 /* Compensate for rounding errors.  */
	    - 0x00800000 /* adjust by one */ ;
  s0 = s0 - u1.i;
  s0 = (int)s0 >= 0 ? s0 : 0;
  s0 >>= 23;

  b <<= s0;
  r1 = 0;

  r0 = 1 << s0;
  a = ((t=a) - b);
  if (a <= t)
    {
      r1 += r0;
      a = ((t=a) - b);
      if (a <= t)
	do {
	  r1 += r0;
	  a = ((t=a) - b);
	} while (a <= t);
    }
  a += b;
  d = b - 1;

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
