/* This is a software fixed-point library.
   Copyright (C) 2007-2014 Free Software Foundation, Inc.

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

/* This implements fixed-point arithmetic.

   Contributed by Chao-ying Fu  <fu@mips.com>.  */

/* To use this file, we need to define one of the following:
   QQ_MODE, UQQ_MODE, HQ_MODE, UHQ_MODE, SQ_MODE, USQ_MODE, DQ_MODE, UDQ_MODE,
   TQ_MODE, UTQ_MODE, HA_MODE, UHA_MODE, SA_MODE, USA_MODE, DA_MODE, UDA_MODE,
   TA_MODE, UTA_MODE.
   Then, all operators for this machine mode will be created.

   Or, we need to define FROM_* TO_* for conversions from one mode to another
   mode.  The mode could be one of the following:
   Fract: QQ, UQQ, HQ, UHQ, SQ, USQ, DQ, UDQ, TQ, UTQ
   Accum: HA, UHA, SA, USA, DA, UDA, TA, UTA
   Signed integer: QI, HI, SI, DI, TI
   Unsigned integer: UQI, UHI, USI, UDI, UTI
   Floating-point: SF, DF
   Ex: If we define FROM_QQ and TO_SI, the conversion from QQ to SI is
   generated.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

#ifndef MIN_UNITS_PER_WORD
#define MIN_UNITS_PER_WORD UNITS_PER_WORD
#endif

#include "fixed-bit.h"

#if defined(FIXED_ADD) && defined(L_add)
FIXED_C_TYPE
FIXED_ADD (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x + y;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_ADD */

#if defined(FIXED_SSADD) && defined(L_ssadd)
FIXED_C_TYPE
FIXED_SSADD (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x + (UINT_C_TYPE) y;
  if ((((x ^ y) >> I_F_BITS) & 1) == 0)
    {
      if (((z ^ x) >> I_F_BITS) & 1)
        {
	  z = ((UINT_C_TYPE) 1) << I_F_BITS;
	  if (x >= 0)
	    z -= (UINT_C_TYPE) 1;
        }
    }
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_SSADD */

#if defined(FIXED_USADD) && defined(L_usadd)
FIXED_C_TYPE
FIXED_USADD (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x + y;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  if (z < x || z < y) /* max */
    {
       z = -1;
#if HAVE_PADDING_BITS
       z = z << PADDING_BITS;
       z = z >> PADDING_BITS;
#endif
    }
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_USADD */

#if defined(FIXED_SUB) && defined(L_sub)
FIXED_C_TYPE
FIXED_SUB (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x - y;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_SUB */

#if defined(FIXED_SSSUB) && defined(L_sssub)
FIXED_C_TYPE
FIXED_SSSUB (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x - (UINT_C_TYPE) y;
  if (((x ^ y) >> I_F_BITS) & 1)
    {
      if (((z ^ x) >> I_F_BITS) & 1)
        {
	  z = ((UINT_C_TYPE) 1) << I_F_BITS;
	  if (x >= 0)
	    z -= (UINT_C_TYPE) 1;
        }
    }
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_SSSUB */

#if defined(FIXED_USSUB) && defined(L_ussub)
FIXED_C_TYPE
FIXED_USSUB (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  z = x - y;
  if (x < y)
    z = 0;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_USSUB */

#if defined(FIXED_SATURATE1) && defined(L_saturate1)
void
FIXED_SATURATE1 (DINT_C_TYPE *a)
{
  DINT_C_TYPE max, min;
  max = (DINT_C_TYPE)1 << I_F_BITS;
  max = max - 1;
#if MODE_UNSIGNED == 0
  min = (DINT_C_TYPE)1 << (2 * FIXED_WIDTH - 1);
  min = min >> (2 * FIXED_WIDTH - 1 - I_F_BITS);
#else
  min = 0;
#endif
  if (*a > max)
    *a = max;
  else if (*a < min)
    *a = min;
}
#endif /* FIXED_SATURATE1 */

#if defined(FIXED_SATURATE2) && defined(L_saturate2)
void
FIXED_SATURATE2 (INT_C_TYPE *high, INT_C_TYPE *low)
{
  INT_C_TYPE r_max, s_max, r_min, s_min;
  r_max = 0;
#if (MODE_UNSIGNED == 0) || HAVE_PADDING_BITS
  s_max = (INT_C_TYPE)1 << I_F_BITS;
  s_max = s_max - 1;
#else
  s_max = -1;
#endif
#if MODE_UNSIGNED == 0
  r_min = -1;
  s_min = (INT_C_TYPE)1 << (FIXED_WIDTH - 1);
  s_min = s_min >> (FIXED_WIDTH - 1 - I_F_BITS);
#else
  r_min = 0;
  s_min = 0;
#endif

  if (*high > r_max
      || (*high == r_max && (UINT_C_TYPE)(*low) > (UINT_C_TYPE)s_max))
    {
      *high = r_max;
      *low = s_max;
    }
  else if (*high < r_min ||
	   (*high == r_min && (UINT_C_TYPE)(*low) < (UINT_C_TYPE)s_min))
    {
      *high = r_min;
      *low = s_min;
    }
}
#endif /* FIXED_SATURATE2 */

#if defined(FIXED_MULHELPER) && defined(L_mulhelper)
FIXED_C_TYPE
FIXED_MULHELPER (FIXED_C_TYPE a, FIXED_C_TYPE b, word_type satp)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y;

#if defined (DINT_C_TYPE)
  INT_C_TYPE z;
  DINT_C_TYPE dx, dy, dz;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  dx = (DINT_C_TYPE) x;
  dy = (DINT_C_TYPE) y;
  dz = dx * dy;
  /* Round the result by adding (1 << (FBITS -1)).  */
  dz += ((DINT_C_TYPE) 1 << (FBITS - 1));
  dz = dz >> FBITS;
  if (satp)
    FIXED_SATURATE1 (&dz);

  z = (INT_C_TYPE) dz;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;

#else /* No DINT_C_TYPE */
  /* The result of multiplication expands to two INT_C_TYPE.  */
  INTunion aa, bb;
  INTunion a_high, a_low, b_high, b_low;
  INTunion high_high, high_low, low_high, low_low;
  INTunion r, s, temp1, temp2;
  INT_C_TYPE carry = 0;
  INT_C_TYPE z;

  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);

  /* Decompose a and b.  */
  aa.ll = x;
  bb.ll = y;

  a_high.s.low = aa.s.high;
  a_high.s.high = 0;
  a_low.s.low = aa.s.low;
  a_low.s.high = 0;
  b_high.s.low = bb.s.high;
  b_high.s.high = 0;
  b_low.s.low = bb.s.low;
  b_low.s.high = 0;

  /* Perform four multiplications.  */
  low_low.ll = a_low.ll * b_low.ll;
  low_high.ll = a_low.ll * b_high.ll;
  high_low.ll = a_high.ll * b_low.ll;
  high_high.ll = a_high.ll * b_high.ll;

  /* Accumulate four results to {r, s}.  */
  temp1.s.high = high_low.s.low;
  temp1.s.low = 0;
  s.ll = low_low.ll + temp1.ll;
  if ((UINT_C_TYPE) s.ll < (UINT_C_TYPE) low_low.ll
      || (UINT_C_TYPE) s.ll < (UINT_C_TYPE) temp1.ll)
    carry ++; /* Carry.  */
  temp1.ll = s.ll;
  temp2.s.high = low_high.s.low;
  temp2.s.low = 0;
  s.ll = temp1.ll + temp2.ll;
  if ((UINT_C_TYPE) s.ll < (UINT_C_TYPE) temp1.ll
      || (UINT_C_TYPE) s.ll < (UINT_C_TYPE) temp2.ll)
    carry ++; /* Carry.  */

  temp1.s.low = high_low.s.high;
  temp1.s.high = 0;
  r.ll = high_high.ll + temp1.ll;
  temp1.s.low = low_high.s.high;
  temp1.s.high = 0;
  r.ll = r.ll + temp1.ll + carry;

#if MODE_UNSIGNED == 0
  /* For signed types, we need to add neg(y) to r, if x < 0.  */
  if (x < 0)
    r.ll = r.ll - y;
  /* We need to add neg(x) to r, if y < 0.  */
  if (y < 0)
    r.ll = r.ll - x;
#endif

  /* Round the result by adding (1 << (FBITS -1)).  */
  temp1.ll = s.ll;
  s.ll += ((INT_C_TYPE) 1 << (FBITS -1));
  if ((UINT_C_TYPE) s.ll < (UINT_C_TYPE) temp1.ll
      || (UINT_C_TYPE) s.ll < (UINT_C_TYPE) ((INT_C_TYPE) 1 << (FBITS -1)))
    r.ll += 1;

  /* Shift right the result by FBITS.  */
#if FBITS == FIXED_WIDTH
  /* This happens only for unsigned types without any padding bits.
     So, it is safe to set r.ll to 0 as it is logically shifted right.  */
  s.ll = r.ll;
  r.ll = 0;
#else
  s.ll = ((UINT_C_TYPE)s.ll) >> FBITS;
  temp1.ll = r.ll << (FIXED_WIDTH - FBITS);
  s.ll = s.ll | temp1.ll;
  r.ll = r.ll >> FBITS;
#endif

  if (satp)
    FIXED_SATURATE2 (&r.ll, &s.ll);

  z = (INT_C_TYPE) s.ll;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
#endif
}
#endif /* FIXED_MULHELPER */

#if defined(FIXED_MUL) && defined(L_mul)
FIXED_C_TYPE
FIXED_MUL (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_MULHELPER (a, b, 0);
}
#endif /* FIXED_MUL */

#if defined(FIXED_SSMUL) && defined(L_ssmul)
FIXED_C_TYPE
FIXED_SSMUL (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_MULHELPER (a, b, 1);
}
#endif /* FIXED_SSMUL */

#if defined(FIXED_USMUL) && defined(L_usmul)
FIXED_C_TYPE
FIXED_USMUL (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_MULHELPER (a, b, 1);
}
#endif /* FIXED_USMUL */

#if defined(FIXED_DIVHELPER) && defined(L_divhelper)
FIXED_C_TYPE
FIXED_DIVHELPER (FIXED_C_TYPE a, FIXED_C_TYPE b, word_type satp)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y;
  INT_C_TYPE z;

#if defined (DINT_C_TYPE)
  DINT_C_TYPE dx, dy, dz;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  dx = (DINT_C_TYPE) x;
  dy = (DINT_C_TYPE) y;
  dx = dx << FBITS;
  dz = dx / dy;
  if (satp)
    FIXED_SATURATE1 (&dz);
  z = (INT_C_TYPE) dz;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;

#else /* No DINT_C_TYPE */
  INT_C_TYPE pos_a, pos_b, r, s;
  INT_C_TYPE quo_r, quo_s, mod, temp;
  word_type i;
#if MODE_UNSIGNED == 0
  word_type num_of_neg = 0;
#endif

  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);
  pos_a = x;
  pos_b = y;

#if MODE_UNSIGNED == 0
  /* If a < 0, negate a.  */
  if (pos_a < 0)
    {
      pos_a = -pos_a;
      num_of_neg ++;
    }
  /* If b < 0, negate b.  */
  if (pos_b < 0)
    {
      pos_b = -pos_b;
      num_of_neg ++;
    }
#endif

  /* Left shift pos_a to {r, s} by FBITS.  */
#if FBITS == FIXED_WIDTH
  /* This happens only for unsigned types without any padding bits.  */
  r = pos_a;
  s = 0;
#else
  s = pos_a << FBITS;
  r = pos_a >> (FIXED_WIDTH - FBITS);
#endif

  /* Unsigned divide r by pos_b to quo_r.  The remainder is in mod.  */
  quo_r = (UINT_C_TYPE)r / (UINT_C_TYPE)pos_b;
  mod = (UINT_C_TYPE)r % (UINT_C_TYPE)pos_b;
  quo_s = 0;

  for (i = 0; i < FIXED_WIDTH; i++)
    {
      /* Record the leftmost bit of mod.  */
      word_type leftmost_mode = (mod >> (FIXED_WIDTH - 1)) & 1;
      /* Shift left mod by 1 bit.  */
      mod = mod << 1;
      /* Test the leftmost bit of s to add to mod.  */
      if ((s >> (FIXED_WIDTH - 1)) & 1)
	mod ++;
      /* Shift left quo_s by 1 bit.  */
      quo_s = quo_s << 1;
      /* Try to calculate (mod - pos_b).  */
      temp = mod - pos_b;
      if (leftmost_mode || (UINT_C_TYPE)mod >= (UINT_C_TYPE)pos_b)
	{
	  quo_s ++;
	  mod = temp;
	}
      /* Shift left s by 1 bit.  */
      s = s << 1;
    }

#if MODE_UNSIGNED == 0
    if (num_of_neg == 1)
      {
	quo_s = -quo_s;
	if (quo_s == 0)
	  quo_r = -quo_r;
	else
	  quo_r = ~quo_r;
      }
#endif
  if (satp)
    FIXED_SATURATE2 (&quo_r, &quo_s);
  z = quo_s;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
#endif
}
#endif /* FIXED_DIVHELPER */

#if defined(FIXED_DIV) && defined(L_div)
FIXED_C_TYPE
FIXED_DIV (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_DIVHELPER (a, b, 0);
}
#endif /* FIXED_DIV */


#if defined(FIXED_UDIV) && defined(L_udiv)
FIXED_C_TYPE
FIXED_UDIV (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_DIVHELPER (a, b, 0);
}
#endif /* FIXED_UDIV */

#if defined(FIXED_SSDIV) && defined(L_ssdiv)
FIXED_C_TYPE
FIXED_SSDIV (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_DIVHELPER (a, b, 1);
}
#endif /* FIXED_SSDIV */

#if defined(FIXED_USDIV) && defined(L_usdiv)
FIXED_C_TYPE
FIXED_USDIV (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  return FIXED_DIVHELPER (a, b, 1);
}
#endif /* FIXED_USDIV */

#if defined(FIXED_NEG) && defined(L_neg)
FIXED_C_TYPE
FIXED_NEG (FIXED_C_TYPE a)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, z;
  memcpy (&x, &a, FIXED_SIZE);
  z = -x;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_NEG */

#if defined(FIXED_SSNEG) && defined(L_ssneg)
FIXED_C_TYPE
FIXED_SSNEG (FIXED_C_TYPE a)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, y, z;
  memcpy (&y, &a, FIXED_SIZE);
  x = 0;
  z = x - (UINT_C_TYPE) y;
  if (((x ^ y) >> I_F_BITS) & 1)
    {
      if (((z ^ x) >> I_F_BITS) & 1)
	z = (((UINT_C_TYPE) 1) << I_F_BITS) - 1;
    }
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_SSNEG */

#if defined(FIXED_USNEG) && defined(L_usneg)
FIXED_C_TYPE
FIXED_USNEG (FIXED_C_TYPE a __attribute__ ((__unused__)))
{
  FIXED_C_TYPE c;
  INT_C_TYPE z;
  z = 0;
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_USNEG */

#if defined(FIXED_ASHLHELPER) && defined(L_ashlhelper)
FIXED_C_TYPE
FIXED_ASHLHELPER (FIXED_C_TYPE a, word_type b, word_type satp)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, z;

#if defined (DINT_C_TYPE)
  DINT_C_TYPE dx, dz;
  memcpy (&x, &a, FIXED_SIZE);
  dx = (DINT_C_TYPE) x;
  if (b >= FIXED_WIDTH)
    dz = dx << FIXED_WIDTH;
  else
    dz = dx << b;
  if (satp)
    FIXED_SATURATE1 (&dz);
  z = (INT_C_TYPE) dz;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;

#else /* No DINT_C_TYPE */
  INT_C_TYPE r, s;
  memcpy (&x, &a, FIXED_SIZE);
  /* We need to shift left x by b bits to {r, s}.  */
  if (b >= FIXED_WIDTH)
    {
      r = b;
      s = 0;
    }
  else
    {
      s = x << b;
      r = x >> (FIXED_WIDTH - b);
    }
  if (satp)
    FIXED_SATURATE2 (&r, &s);
  z = s;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
#endif
}
#endif /* FIXED_ASHLHELPER */

#if defined(FIXED_ASHL) && defined(L_ashl)
FIXED_C_TYPE
FIXED_ASHL (FIXED_C_TYPE a, word_type b)
{
  return FIXED_ASHLHELPER (a, b, 0);
}
#endif /* FIXED_ASHL */

#if defined(FIXED_ASHR) && defined(L_ashr)
FIXED_C_TYPE
FIXED_ASHR (FIXED_C_TYPE a, word_type b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, z;
  memcpy (&x, &a, FIXED_SIZE);
  z = x >> b;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_ASHR */

#if defined(FIXED_LSHR) && defined(L_lshr)
FIXED_C_TYPE
FIXED_LSHR (FIXED_C_TYPE a, word_type b)
{
  FIXED_C_TYPE c;
  INT_C_TYPE x, z;
  memcpy (&x, &a, FIXED_SIZE);
  z = x >> b;
#if HAVE_PADDING_BITS
  z = z << PADDING_BITS;
  z = z >> PADDING_BITS;
#endif
  memcpy (&c, &z, FIXED_SIZE);
  return c;
}
#endif /* FIXED_LSHR */

#if defined(FIXED_SSASHL) && defined(L_ssashl)
FIXED_C_TYPE
FIXED_SSASHL (FIXED_C_TYPE a, word_type b)
{
  return FIXED_ASHLHELPER (a, b, 1);
}
#endif /* FIXED_SSASHL */

#if defined(FIXED_USASHL) && defined(L_usashl)
FIXED_C_TYPE
FIXED_USASHL (FIXED_C_TYPE a, word_type b)
{
  return FIXED_ASHLHELPER (a, b, 1);
}
#endif /* FIXED_USASHL */

#if defined(FIXED_CMP) && defined(L_cmp)
word_type
FIXED_CMP (FIXED_C_TYPE a, FIXED_C_TYPE b)
{
  INT_C_TYPE x, y;
  memcpy (&x, &a, FIXED_SIZE);
  memcpy (&y, &b, FIXED_SIZE);

  if (x < y)
    return 0;
  else if (x > y)
    return 2;

  return 1;
}
#endif /* FIXED_CMP */

/* Fixed -> Fixed.  */
#if defined(FRACT) && defined(L_fract) && FROM_TYPE == 4 && TO_TYPE == 4
TO_FIXED_C_TYPE
FRACT (FROM_FIXED_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  FROM_INT_C_TYPE x;
  TO_INT_C_TYPE z;
  int shift_amount;
  memcpy (&x, &a, FROM_FIXED_SIZE);
#if TO_FBITS > FROM_FBITS  /* Need left shift.  */
  shift_amount = TO_FBITS - FROM_FBITS;
  z = (TO_INT_C_TYPE) x;
  z = z << shift_amount;
#else /* TO_FBITS <= FROM_FBITS.  Need right Shift.  */
  shift_amount = FROM_FBITS - TO_FBITS;
  x = x >> shift_amount;
  z = (TO_INT_C_TYPE) x;
#endif /* TO_FBITS > FROM_FBITS  */

#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* FRACT && FROM_TYPE == 4 && TO_TYPE == 4  */

/* Fixed -> Fixed with saturation.  */
#if defined(SATFRACT) && defined(L_satfract) && FROM_TYPE == 4 && TO_TYPE == 4
TO_FIXED_C_TYPE
SATFRACT (FROM_FIXED_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  TO_INT_C_TYPE z;
  FROM_INT_C_TYPE x;
#if FROM_MODE_UNSIGNED == 0
  BIG_SINT_C_TYPE high, low;
  BIG_SINT_C_TYPE max_high, max_low;
#if TO_MODE_UNSIGNED == 0
  BIG_SINT_C_TYPE min_high, min_low;
#endif
#else
  BIG_UINT_C_TYPE high, low;
  BIG_UINT_C_TYPE max_high, max_low;
#endif
#if TO_FBITS > FROM_FBITS
  BIG_UINT_C_TYPE utemp;
#endif
#if TO_MODE_UNSIGNED == 0
  BIG_SINT_C_TYPE stemp;
#endif
#if TO_FBITS != FROM_FBITS
  int shift_amount;
#endif
  memcpy (&x, &a, FROM_FIXED_SIZE);

  /* Step 1. We need to store x to {high, low}.  */
#if FROM_MODE_UNSIGNED == 0
  low = (BIG_SINT_C_TYPE) x;
  if (x < 0)
    high = -1;
  else
    high = 0;
#else
  low = (BIG_UINT_C_TYPE) x;
  high = 0;
#endif

  /* Step 2. We need to shift {high, low}.  */
#if TO_FBITS > FROM_FBITS /* Left shift.  */
  shift_amount = TO_FBITS - FROM_FBITS;
  utemp = (BIG_UINT_C_TYPE) low;
  utemp = utemp >> (BIG_WIDTH - shift_amount);
  high = ((BIG_UINT_C_TYPE)(high << shift_amount)) | utemp;
  low = low << shift_amount;
#elif TO_FBITS < FROM_FBITS /* Right shift.  */
  shift_amount = FROM_FBITS - TO_FBITS;
  low = low >> shift_amount;
#endif

  /* Step 3. Compare {high, low} with max and  min of TO_FIXED_C_TYPE.  */
  max_high = 0;
#if BIG_WIDTH > TO_FIXED_WIDTH || TO_MODE_UNSIGNED == 0 || TO_HAVE_PADDING_BITS
  max_low = (BIG_UINT_C_TYPE)1 << TO_I_F_BITS;
  max_low = max_low - 1;
#else
  max_low = -1;
#endif

#if TO_MODE_UNSIGNED == 0
  stemp = (BIG_SINT_C_TYPE)1 << (BIG_WIDTH - 1);
  stemp = stemp >> (BIG_WIDTH - 1 - TO_I_F_BITS);
#if FROM_MODE_UNSIGNED == 0
  min_high = -1;
  min_low = stemp;
#endif
#endif

#if FROM_MODE_UNSIGNED == 0 && TO_MODE_UNSIGNED == 0
  /* Signed -> Signed.  */
  if ((BIG_SINT_C_TYPE) high > (BIG_SINT_C_TYPE) max_high
      || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) max_high
	  && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
  else if ((BIG_SINT_C_TYPE) high < (BIG_SINT_C_TYPE) min_high
	   || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) min_high
	       && (BIG_UINT_C_TYPE) low < (BIG_UINT_C_TYPE) min_low))
    low = min_low; /* Minimum.  */
#elif FROM_MODE_UNSIGNED == 1 && TO_MODE_UNSIGNED == 1
  /* Unigned -> Unsigned.  */
  if ((BIG_UINT_C_TYPE) high > (BIG_UINT_C_TYPE) max_high
      || ((BIG_UINT_C_TYPE) high == (BIG_UINT_C_TYPE) max_high
	  && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#elif FROM_MODE_UNSIGNED == 0 && TO_MODE_UNSIGNED == 1
  /* Signed -> Unsigned.  */
  if (x < 0)
    low = 0; /* Minimum.  */
  else if ((BIG_UINT_C_TYPE) high > (BIG_UINT_C_TYPE) max_high
	   || ((BIG_UINT_C_TYPE) high == (BIG_UINT_C_TYPE) max_high
	       && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#elif FROM_MODE_UNSIGNED == 1 && TO_MODE_UNSIGNED == 0
  /* Unsigned -> Signed.  */
  if ((BIG_SINT_C_TYPE) high < 0)
    low = max_low; /* Maximum.  */
  else if ((BIG_SINT_C_TYPE) high > (BIG_SINT_C_TYPE) max_high
	   || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) max_high
	       && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#endif

  /* Step 4. Store the result.  */
  z = (TO_INT_C_TYPE) low;
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(SATFRACT) && FROM_TYPE == 4 && TO_TYPE == 4  */

/* Fixed -> Int.  */
#if defined(FRACT) && defined(L_fract) && FROM_TYPE == 4 && TO_TYPE == 1
TO_INT_C_TYPE
FRACT (FROM_FIXED_C_TYPE a)
{
  FROM_INT_C_TYPE x;
  TO_INT_C_TYPE z;
  FROM_INT_C_TYPE i = 0;
  memcpy (&x, &a, FROM_FIXED_SIZE);

#if FROM_MODE_UNSIGNED == 0
  if (x < 0)
    {
#if FROM_FIXED_WIDTH == FROM_FBITS
      if (x != 0)
	i = 1;
#else
      if (((FROM_INT_C_TYPE)(x << (FROM_FIXED_WIDTH - FROM_FBITS))) != 0)
	i = 1;
#endif
    }
#endif

#if FROM_FIXED_WIDTH == FROM_FBITS
  x = 0;
#else
  x = x >> FROM_FBITS;
#endif
  x = x + i;
  z = (TO_INT_C_TYPE) x;
  return z;
}
#endif /* defined(FRACT) && FROM_TYPE == 4 && TO_TYPE == 1  */

/* Fixed -> Unsigned int.  */
#if defined(FRACTUNS) && defined(L_fractuns) && FROM_TYPE == 4 && TO_TYPE == 2
TO_INT_C_TYPE
FRACTUNS (FROM_FIXED_C_TYPE a)
{
  FROM_INT_C_TYPE x;
  TO_INT_C_TYPE z;
  FROM_INT_C_TYPE i = 0;
  memcpy (&x, &a, FROM_FIXED_SIZE);

#if FROM_MODE_UNSIGNED == 0
  if (x < 0)
    {
#if FROM_FIXED_WIDTH == FROM_FBITS
      if (x != 0)
	i = 1;
#else
      if (((FROM_INT_C_TYPE)(x << (FROM_FIXED_WIDTH - FROM_FBITS))) != 0)
	i = 1;
#endif
    }
#endif

#if FROM_FIXED_WIDTH == FROM_FBITS
  x = 0;
#else
  x = x >> FROM_FBITS;
#endif
  x = x + i;
  z = (TO_INT_C_TYPE) x;
  return z;
}
#endif /* defined(FRACTUNS) && FROM_TYPE == 4 && TO_TYPE == 2  */

/* Int -> Fixed.  */
#if defined(FRACT) && defined(L_fract) && FROM_TYPE == 1 && TO_TYPE == 4
TO_FIXED_C_TYPE
FRACT (FROM_INT_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  TO_INT_C_TYPE z;
  z = (TO_INT_C_TYPE) a;
#if TO_FIXED_WIDTH == TO_FBITS
  z = 0;
#else
  z = z << TO_FBITS;
#endif
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(FRACT) && FROM_TYPE == 1 && TO_TYPE == 4  */

/* Signed int -> Fixed with saturation.  */
#if defined(SATFRACT) && defined(L_satfract) && FROM_TYPE == 1 && TO_TYPE == 4
TO_FIXED_C_TYPE
SATFRACT (FROM_INT_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  TO_INT_C_TYPE z;
  FROM_INT_C_TYPE x = a;
  BIG_SINT_C_TYPE high, low;
  BIG_SINT_C_TYPE max_high, max_low;
#if TO_MODE_UNSIGNED == 0
  BIG_SINT_C_TYPE min_high, min_low;
  BIG_SINT_C_TYPE stemp;
#endif
#if BIG_WIDTH != TO_FBITS
  BIG_UINT_C_TYPE utemp;
  int shift_amount;
#endif

  /* Step 1. We need to store x to {high, low}.  */
  low = (BIG_SINT_C_TYPE) x;
  if (x < 0)
    high = -1;
  else
    high = 0;

  /* Step 2. We need to left shift {high, low}.  */
#if BIG_WIDTH == TO_FBITS
  high = low;
  low = 0;
#else
  shift_amount = TO_FBITS;
  utemp = (BIG_UINT_C_TYPE) low;
  utemp = utemp >> (BIG_WIDTH - shift_amount);
  high = ((BIG_UINT_C_TYPE)(high << shift_amount)) | utemp;
  low = low << shift_amount;
#endif

  /* Step 3. Compare {high, low} with max and  min of TO_FIXED_C_TYPE.  */
  max_high = 0;
#if BIG_WIDTH > TO_FIXED_WIDTH || TO_MODE_UNSIGNED == 0 || TO_HAVE_PADDING_BITS
  max_low = (BIG_UINT_C_TYPE)1 << TO_I_F_BITS;
  max_low = max_low - 1;
#else
  max_low = -1;
#endif

#if TO_MODE_UNSIGNED == 0
  min_high = -1;
  stemp = (BIG_SINT_C_TYPE)1 << (BIG_WIDTH - 1);
  stemp = stemp >> (BIG_WIDTH - 1 - TO_I_F_BITS);
  min_low = stemp;

  /* Signed -> Signed.  */
  if ((BIG_SINT_C_TYPE) high > (BIG_SINT_C_TYPE) max_high
      || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) max_high
          && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
  else if ((BIG_SINT_C_TYPE) high < (BIG_SINT_C_TYPE) min_high
           || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) min_high
               && (BIG_UINT_C_TYPE) low < (BIG_UINT_C_TYPE) min_low))
    low = min_low; /* Minimum.  */
#else
  /* Signed -> Unsigned.  */
  if (x < 0)
    low = 0; /* Minimum.  */
  else if ((BIG_UINT_C_TYPE) high > (BIG_UINT_C_TYPE) max_high
           || ((BIG_UINT_C_TYPE) high == (BIG_UINT_C_TYPE) max_high
               && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#endif

  /* Step 4. Store the result.  */
  z = (TO_INT_C_TYPE) low;
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(SATFRACT) && FROM_TYPE == 1 && TO_TYPE == 4  */

/* Unsigned int -> Fixed.  */
#if defined(FRACTUNS) && defined(L_fractuns) &&FROM_TYPE == 2 && TO_TYPE == 4
TO_FIXED_C_TYPE
FRACTUNS (FROM_INT_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  TO_INT_C_TYPE z;
  z = (TO_INT_C_TYPE) a;
#if TO_FIXED_WIDTH == TO_FBITS
  z = 0;
#else
  z = z << TO_FBITS;
#endif
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(FRACTUNS) && FROM_TYPE == 2 && TO_TYPE == 4  */

/* Unsigned int -> Fixed with saturation.  */
#if defined(SATFRACTUNS) && defined(L_satfractuns) && FROM_TYPE == 2 && TO_TYPE == 4
TO_FIXED_C_TYPE
SATFRACTUNS (FROM_INT_C_TYPE a)
{
  TO_FIXED_C_TYPE c;
  TO_INT_C_TYPE z;
  FROM_INT_C_TYPE x = a;
  BIG_UINT_C_TYPE high, low;
  BIG_UINT_C_TYPE max_high, max_low;
#if BIG_WIDTH != TO_FBITS
  BIG_UINT_C_TYPE utemp;
  int shift_amount;
#endif

  /* Step 1. We need to store x to {high, low}.  */
  low = (BIG_UINT_C_TYPE) x;
  high = 0;

  /* Step 2. We need to left shift {high, low}.  */
#if BIG_WIDTH == TO_FBITS
  high = low;
  low = 0;
#else
  shift_amount = TO_FBITS;
  utemp = (BIG_UINT_C_TYPE) low;
  utemp = utemp >> (BIG_WIDTH - shift_amount);
  high = ((BIG_UINT_C_TYPE)(high << shift_amount)) | utemp;
  low = low << shift_amount;
#endif

  /* Step 3. Compare {high, low} with max and  min of TO_FIXED_C_TYPE.  */
  max_high = 0;
#if BIG_WIDTH > TO_FIXED_WIDTH || TO_MODE_UNSIGNED == 0 || TO_HAVE_PADDING_BITS
  max_low = (BIG_UINT_C_TYPE)1 << TO_I_F_BITS;
  max_low = max_low - 1;
#else
  max_low = -1;
#endif

#if TO_MODE_UNSIGNED == 1
  /* Unigned -> Unsigned.  */
  if ((BIG_UINT_C_TYPE) high > (BIG_UINT_C_TYPE) max_high
      || ((BIG_UINT_C_TYPE) high == (BIG_UINT_C_TYPE) max_high
          && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#else
  /* Unsigned -> Signed.  */
  if ((BIG_SINT_C_TYPE) high < 0)
    low = max_low; /* Maximum.  */
  else if ((BIG_SINT_C_TYPE) high > (BIG_SINT_C_TYPE) max_high
           || ((BIG_SINT_C_TYPE) high == (BIG_SINT_C_TYPE) max_high
               && (BIG_UINT_C_TYPE) low > (BIG_UINT_C_TYPE) max_low))
    low = max_low; /* Maximum.  */
#endif

  /* Step 4. Store the result.  */
  z = (TO_INT_C_TYPE) low;
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(SATFRACTUNS) && FROM_TYPE == 2 && TO_TYPE == 4  */

/* Fixed -> Float.  */
#if defined(FRACT) && defined(L_fract) && FROM_TYPE == 4 && TO_TYPE == 3
TO_FLOAT_C_TYPE
FRACT (FROM_FIXED_C_TYPE a)
{
  FROM_INT_C_TYPE x;
  TO_FLOAT_C_TYPE z;
  memcpy (&x, &a, FROM_FIXED_SIZE);
  z = (TO_FLOAT_C_TYPE) x;
  z = z / BASE;
  return z;
}
#endif /* defined(FRACT) && FROM_TYPE == 4 && TO_TYPE == 3  */

/* Float -> Fixed.  */
#if defined(FRACT) && defined(L_fract) && FROM_TYPE == 3 && TO_TYPE == 4
TO_FIXED_C_TYPE
FRACT (FROM_FLOAT_C_TYPE a)
{
  FROM_FLOAT_C_TYPE temp;
  TO_INT_C_TYPE z;
  TO_FIXED_C_TYPE c;

  temp = a * BASE;
  z = (TO_INT_C_TYPE) temp;
#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(FRACT) && FROM_TYPE == 3 && TO_TYPE == 4  */

/* Float -> Fixed with saturation.  */
#if defined(SATFRACT) && defined(L_satfract) && FROM_TYPE == 3 && TO_TYPE == 4
TO_FIXED_C_TYPE
SATFRACT (FROM_FLOAT_C_TYPE a)
{
  FROM_FLOAT_C_TYPE temp;
  TO_INT_C_TYPE z;
  TO_FIXED_C_TYPE c;

  if (a >= FIXED_MAX)
    {
#if TO_MODE_UNSIGNED == 0 || TO_HAVE_PADDING_BITS
      z = (TO_INT_C_TYPE)1 << TO_I_F_BITS;
      z = z - 1;
#else
      z = -1;
#endif
    }
  else if (a <= FIXED_MIN)
    {
#if TO_MODE_UNSIGNED == 0
      z = (TO_INT_C_TYPE)1 << TO_I_F_BITS;
#else
      z = 0;
#endif
    }
  else
    {
      temp = a * BASE;
      z = (TO_INT_C_TYPE) temp;
    }

#if TO_HAVE_PADDING_BITS
  z = z << TO_PADDING_BITS;
  z = z >> TO_PADDING_BITS;
#endif
  memcpy (&c, &z, TO_FIXED_SIZE);
  return c;
}
#endif /* defined(SATFRACT) && FROM_TYPE == 3 && TO_TYPE == 4  */

