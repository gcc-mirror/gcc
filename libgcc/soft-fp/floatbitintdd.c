/* Software floating-point emulation.
   Convert a _BitInt to _Decimal64.

   Copyright (C) 2023 Free Software Foundation, Inc.

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

#include "soft-fp.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
extern _Decimal64 __bid_floatbitintdd (const UBILtype *, SItype);

_Decimal64
__bid_floatbitintdd (const UBILtype *i, SItype iprec)
{
  iprec = bitint_reduce_prec (&i, iprec);
  USItype aiprec = iprec < 0 ? -iprec : iprec;
  USItype in = (aiprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  USItype idx = BITINT_END (0, in - 1);
  UBILtype msb = i[idx];
  UDItype mantissa;
  SItype exponent = 0;
  UBILtype inexact = 0;
  union { _Decimal64 d; UDItype u; } u, ui;
  if (aiprec % BIL_TYPE_SIZE)
    {
      if (iprec > 0)
	msb &= ((UBILtype) 1 << (aiprec % BIL_TYPE_SIZE)) - 1;
      else
	msb |= (UBILtype) -1 << (aiprec % BIL_TYPE_SIZE);
    }
  if (iprec < 0)
    {
      SItype n = sizeof (0ULL) * __CHAR_BIT__ + 1 - __builtin_clzll (~msb);
      aiprec = (in - 1) * BIL_TYPE_SIZE + n;
    }
  else if (msb == 0)
    aiprec = 1;
  else
    {
      SItype n = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (msb);
      aiprec = (in - 1) * BIL_TYPE_SIZE + n;
    }
  /* Number of bits in (_BitInt(2048)) 9999999999999999e+369DD.  */
  if (aiprec > 1279 + (iprec < 0))
    {
    ovf:
      if (iprec < 0)
	u.d = -9000000000000000e+369DD;
      else
	u.d = 9000000000000000e+369DD;
      __asm ("" : "+g" (u.d));
      u.d += u.d;
      __asm ("" : "+g" (u.d));
      goto done;
    }
  /* Bit precision of 9999999999999999uwb.  */
  if (aiprec >= 54)
    {
      USItype pow10_limbs, q_limbs, q2_limbs, j;
      USItype exp_bits = 0, e;
      UDItype m;
      UBILtype *buf;
      /* First do a possibly large divide smaller enough such that
	 we only need to check remainder for 0 or non-0 and then
	 we'll do further division.  */
      if (aiprec >= 54 + 4 + 10)
	{
	  exp_bits = (aiprec - 54 - 4) / 10;
	  exponent = exp_bits * 3;
	  /* Upper estimate for pow10 (exponent) bits.  */
	  exp_bits = exp_bits * 10 - exp_bits / 30;
	}
      pow10_limbs = (exp_bits + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      /* 72 is the highest number of quotient bits needed on
	 aiprec range of [68, 1279].  E.g. if aiprec is 1277,
	 exponent will be 363 and exp_bits 1206.  1277 - 1206 + 1
	 is 72.  Unfortunately that means the result doesn't fit into
	 UDItype...  */
      q_limbs = (72 + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      q2_limbs = 64 / BIL_TYPE_SIZE;
      buf = __builtin_alloca ((q_limbs + pow10_limbs * 2 + q2_limbs + 2)
			      * sizeof (UBILtype));
      if (exponent)
	{
	  __bid_pow10bitint (buf + q_limbs, exp_bits, exponent);
	  __divmodbitint4 (buf, q_limbs * BIL_TYPE_SIZE,
			   buf + q_limbs + pow10_limbs,
			   pow10_limbs * BIL_TYPE_SIZE,
			   i, iprec < 0 ? -aiprec : aiprec,
			   buf + q_limbs, exp_bits);
	  if (iprec < 0)
	    bitint_negate (buf + BITINT_END (q_limbs - 1, 0),
			   buf + BITINT_END (q_limbs - 1, 0), q_limbs);
	  inexact = buf[q_limbs + pow10_limbs];
	  for (j = 1; j < pow10_limbs; ++j)
	    inexact |= buf[q_limbs + pow10_limbs + j];
	}
      else
	{
	  __builtin_memcpy (buf + BITINT_END (q_limbs - in + 1, 0), i,
			    (in - 1) * sizeof (UBILtype));
	  buf[BITINT_END (q_limbs - in, in - 1)] = msb;
	  if (iprec < 0)
	    bitint_negate (buf + BITINT_END (q_limbs - 1, 0),
			   buf + BITINT_END (q_limbs - 1, 0), in);
	  if (q_limbs > in)
	    __builtin_memset (buf + BITINT_END (0, in), '\0',
			      (q_limbs - in) * sizeof (UBILtype));
	}
      e = 0;
#if BIL_TYPE_SIZE == 64
      m = buf[BITINT_END (1, 0)];
#elif BIL_TYPE_SIZE == 32
      m = (UDItype) buf[1] << 32 | buf[BITINT_END (2, 0)];
#else
# error Unsupported BIL_TYPE_SIZE
#endif
      if (buf[BITINT_END (0, q_limbs - 1)])
	{
	  if (buf[BITINT_END (0, q_limbs - 1)] > 0x5)
	    {
	      /* 1000000000000000000000wb */
	      if (buf[BITINT_END (0, q_limbs - 1)] > 0x36
		  || (buf[BITINT_END (0, q_limbs - 1)] == 0x36
		      && m >= (UDItype) 0x35c9adc5dea00000))
		e = 6;
	      else
		e = 5;
	    }
	  /* 100000000000000000000wb */
	  else if (buf[BITINT_END (0, q_limbs - 1)] == 0x5
		   && m >= (UDItype) 0x6bc75e2d63100000)
	    e = 5;
	  else
	    e = 4;
	}
      else if (m >= (UDItype) 1000000000000000000)
	{
	  if (m >= (UDItype) 10000000000000000000ULL)
	    e = 4;
	  else
	    e = 3;
	}
      else if (m >= (UDItype) 100000000000000000)
	e = 2;
      else if (m >= (UDItype) 10000000000000000)
	e = 1;
      exponent += e;
      if (exponent > 369)
	goto ovf;
      if (e)
	{
	  UBILtype rem, half;
	  __bid_pow10bitint (buf + q_limbs + pow10_limbs * 2,
			     BIL_TYPE_SIZE, e);
	  __divmodbitint4 (buf + q_limbs + pow10_limbs * 2 + 1,
			   q2_limbs * BIL_TYPE_SIZE,
			   buf + q_limbs + pow10_limbs * 2 + 1 + q2_limbs,
			   BIL_TYPE_SIZE,
			   buf, q_limbs * BIL_TYPE_SIZE,
			   buf + q_limbs + pow10_limbs * 2, BIL_TYPE_SIZE);
	  half = buf[q_limbs + pow10_limbs * 2] / 2;
	  rem = buf[q_limbs + pow10_limbs * 2 + 1 + q2_limbs];
	  if (inexact)
	    {
	      /* If first division discovered some non-0 digits
		 and this second division is by 10, e.g.
		 for XXXXXX5499999999999 or XXXXXX5000000000001
		 if first division is by 10^12 and second by 10^1,
		 doing rem |= 1 wouldn't change the 5.  Similarly
		 for rem 4 doing rem |= 1 would change it to 5,
		 but we don't want to change it in that case.  */
	      if (e == 1)
		{
		  if (rem == 5)
		    rem = 6;
		  else if (rem != 4)
		    rem |= 1;
		}
	      else
		rem |= 1;
	    }
	  /* Set inexact to 0, 1, 2, 3 depending on if remainder
	     of the divisions is exact 0, smaller than 10^exponent / 2,
	     exactly 10^exponent / 2 or greater than that.  */
	  if (rem >= half)
	    inexact = 2 + (rem > half);
	  else
	    inexact = (rem != 0);
#if BIL_TYPE_SIZE == 64
	  mantissa = buf[q_limbs + pow10_limbs * 2 + 1];
#else
	  mantissa
	    = ((UDItype)
	       buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (0, 1)] << 32
	       | buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (1, 0)]);
#endif
	}
      else
#if BIL_TYPE_SIZE == 64
	mantissa = buf[BITINT_END (1, 0)];
#else
	mantissa = (UDItype) buf[1] << 32 | buf[BITINT_END (2, 0)];
#endif
    }
  else
    {
#if BIL_TYPE_SIZE == 64
      mantissa = msb;
#else
      if (in == 1)
	mantissa = iprec < 0 ? (UDItype) (BILtype) msb : (UDItype) msb;
      else
	mantissa = (UDItype) msb << 32 | i[BITINT_END (1, 0)];
#endif
      if (iprec < 0)
	mantissa = -mantissa;
    }

  exponent += 398;
  if (mantissa >= (UDItype) 0x20000000000000)
    u.u = (((((iprec < 0) << 2) | (UDItype) 3) << 61)
	   | (((UDItype) exponent) << 51)
	   | (mantissa ^ (UDItype) 0x20000000000000));
  else
    u.u = ((((UDItype) (iprec < 0)) << 63)
	   | (((UDItype) exponent) << 53)
	   | mantissa);
  if (inexact)
    {
      ui.u = ((((UDItype) (iprec < 0)) << 63)
	      | (((UDItype) (exponent - 1)) << 53)
	      | (inexact + 3));
      __asm ("" : "+g" (u.d));
      __asm ("" : "+g" (ui.d));
      u.d += ui.d;
      __asm ("" : "+g" (u.d));
    }

done:
  return u.d;
}
#endif
