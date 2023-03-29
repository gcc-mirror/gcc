/* Copyright (C) 2014-2023 Free Software Foundation, Inc.

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

/* Work out the largest "word" size that we can deal with on this target.  */
#if MIN_UNITS_PER_WORD > 4
# define LIBGCC2_MAX_UNITS_PER_WORD 8
#elif (MIN_UNITS_PER_WORD > 2 \
       || (MIN_UNITS_PER_WORD > 1 && __SIZEOF_LONG_LONG__ > 4))
# define LIBGCC2_MAX_UNITS_PER_WORD 4
#else
# define LIBGCC2_MAX_UNITS_PER_WORD MIN_UNITS_PER_WORD
#endif

/* Work out what word size we are using for this compilation.
   The value can be set on the command line.  */
#ifndef LIBGCC2_UNITS_PER_WORD
#define LIBGCC2_UNITS_PER_WORD LIBGCC2_MAX_UNITS_PER_WORD
#endif

#if LIBGCC2_UNITS_PER_WORD <= LIBGCC2_MAX_UNITS_PER_WORD

#include "libgcc2.h"

/* umul_ppmm(high_prod, low_prod, multiplier, multiplicand) multiplies two
   UWtype integers MULTIPLIER and MULTIPLICAND, and generates a two UWtype
   word product in HIGH_PROD and LOW_PROD.  */

#undef umul_ppmm
#define umul_ppmm(wh, wl, u, v)			\
  do {						\
    /* Generate multu instruction.  */		\
    UDWtype __t = (UDWtype)(u) * (UDWtype)(v);	\
    (wl) = (UWtype)__t;				\
    (wh) = (UWtype)(__t >> W_TYPE_SIZE);	\
  } while (0)

/* sub_ddmmss(high_difference, low_difference, high_minuend, low_minuend,
   high_subtrahend, low_subtrahend) subtracts two two-word UWtype integers,
   composed by HIGH_MINUEND_1 and LOW_MINUEND_1, and HIGH_SUBTRAHEND_2 and
   LOW_SUBTRAHEND_2 respectively.  The result is placed in HIGH_DIFFERENCE
   and LOW_DIFFERENCE.  Overflow (i.e. carry out) is not stored anywhere,
   and is lost.  */

#undef sub_ddmmss
#define sub_ddmmss(sh, sl, ah, al, bh, bl)		\
  __asm__ ("sub.l   %0,%2,%4\n\t"			\
	   "subc.l  %1,%3,%5"				\
	   : "=&r" (sl), "=r" (sh)			\
	   : "r" (al), "r" (ah), "r" (bl), "r" (bh))

/* udiv_qqrnnd(high_quotient, low_quotient, remainder, high_numerator,
   low_numerator, denominator) divides a UDWtype, composed by the UWtype
   HIGH_NUMERATOR and LOW_NUMERATOR, by DENOMINATOR and places the quotient
   in QUOTIENT and the remainder in REMAINDER.  */

#define udiv_qqrnnd(qh, ql, r, nh, nl, d)	\
  __asm__ ("writemd %3,%4\n\t"			\
	   "divdu   %5\n\t"			\
	   "readmda %0\n\t"			\
	   "readmdb %1\n\t"			\
	   "readmdc %2"				\
	   : "=r" (ql), "=r" (qh), "=r" (r)	\
	   : "r" (nl), "r" (nh), "r" (d)	\
	   : "mdb", "mdc")

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
#define L_udivmoddi4
#endif

#ifdef L_udivmoddi4

#if (defined (L_udivdi3) || defined (L_divdi3) || \
     defined (L_umoddi3) || defined (L_moddi3))
static inline __attribute__ ((__always_inline__))
#endif
UDWtype
__udivmoddi4 (UDWtype n, UDWtype d, UDWtype *rp)
{
  const DWunion nn = {.ll = n};
  const DWunion dd = {.ll = d};
  DWunion rr;
  UWtype d0, d1, n0, n1, n2;
  UWtype q0, q1;
  UWtype b, bm;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;

  if (d1 == 0)
    {
      /* qq = NN / 0d */

      if (d0 == 0)
	d0 = 1 / d0;	/* Divide intentionally by zero.  */

      udiv_qqrnnd (q1, q0, n0, n1, n0, d0);

      /* Remainder in n0.  */

      if (rp != 0)
	{
	  rr.s.low = n0;
	  rr.s.high = 0;
	  *rp = rr.ll;
	}
    }

  else
    {
      if (d1 > n1)
	{
	  /* 00 = nn / DD */

	  q0 = 0;
	  q1 = 0;

	  /* Remainder in n1n0.  */
	  if (rp != 0)
	    {
	      rr.s.low = n0;
	      rr.s.high = n1;
	      *rp = rr.ll;
	    }
	}
      else
	{
	  /* 0q = NN / dd */

	  count_leading_zeros (bm, d1);
	  if (bm == 0)
	    {
	      /* From (n1 >= d1) /\ (the most significant bit of d1 is set),
		 conclude (the most significant bit of n1 is set) /\ (the
		 quotient digit q0 = 0 or 1).

		 This special case is necessary, not an optimization.  */

	      /* The condition on the next line takes advantage of that
		 n1 >= d1 (true due to program flow).  */
	      if (n1 > d1 || n0 >= d0)
		{
		  q0 = 1;
		  sub_ddmmss (n1, n0, n1, n0, d1, d0);
		}
	      else
		q0 = 0;

	      q1 = 0;

	      if (rp != 0)
		{
		  rr.s.low = n0;
		  rr.s.high = n1;
		  *rp = rr.ll;
		}
	    }
	  else
	    {
	      UWtype m1, m0;
	      /* Normalize.  */

	      b = W_TYPE_SIZE - bm;

	      d1 = (d1 << bm) | (d0 >> b);
	      d0 = d0 << bm;
	      n2 = n1 >> b;
	      n1 = (n1 << bm) | (n0 >> b);
	      n0 = n0 << bm;

	      udiv_qqrnnd (q1, q0, n1, n2, n1, d1);
	      umul_ppmm (m1, m0, q0, d0);

	      if (m1 > n1 || (m1 == n1 && m0 > n0))
		{
		  q0--;
		  sub_ddmmss (m1, m0, m1, m0, d1, d0);
		}

	      /* Remainder in (n1n0 - m1m0) >> bm.  */
	      if (rp != 0)
		{
		  sub_ddmmss (n1, n0, n1, n0, m1, m0);
		  rr.s.low = (n1 << b) | (n0 >> bm);
		  rr.s.high = n1 >> bm;
		  *rp = rr.ll;
		}
	    }
	}
    }

  const DWunion ww = {{.low = q0, .high = q1}};
  return ww.ll;
}
#endif

#ifdef L_divdi3
DWtype
__divdi3 (DWtype u, DWtype v)
{
  Wtype c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    c = ~c,
    vv.ll = -vv.ll;

  w = __udivmoddi4 (uu.ll, vv.ll, (UDWtype *) 0);
  if (c)
    w = -w;

  return w;
}
#endif

#ifdef L_moddi3
DWtype
__moddi3 (DWtype u, DWtype v)
{
  Wtype c = 0;
  DWunion uu = {.ll = u};
  DWunion vv = {.ll = v};
  DWtype w;

  if (uu.s.high < 0)
    c = ~c,
    uu.ll = -uu.ll;
  if (vv.s.high < 0)
    vv.ll = -vv.ll;

  (void) __udivmoddi4 (uu.ll, vv.ll, (UDWtype*)&w);
  if (c)
    w = -w;

  return w;
}
#endif

#ifdef L_umoddi3
UDWtype
__umoddi3 (UDWtype u, UDWtype v)
{
  UDWtype w;

  (void) __udivmoddi4 (u, v, &w);

  return w;
}
#endif

#ifdef L_udivdi3
UDWtype
__udivdi3 (UDWtype n, UDWtype d)
{
  return __udivmoddi4 (n, d, (UDWtype *) 0);
}
#endif

#ifdef L_set_trampoline_parity
#undef int
extern void __set_trampoline_parity (UWtype *);

static inline UWtype
parity_bit (UWtype x)
{
  x ^= x << 16;
  x ^= x << 8;
  x ^= x << 4;
  x ^= x << 2;
  x ^= x << 1;
  return x & ((UWtype) 1 << (W_TYPE_SIZE - 1));
}

void
__set_trampoline_parity (UWtype *addr)
{
  int i;

  for (i = 0;
       i < (__LIBGCC_TRAMPOLINE_SIZE__ * __CHAR_BIT__) / W_TYPE_SIZE;
       i++)
    addr[i] |= parity_bit (addr[i]);
}
#endif

#endif /* LIBGCC2_UNITS_PER_WORD <= MIN_UNITS_PER_WORD */
