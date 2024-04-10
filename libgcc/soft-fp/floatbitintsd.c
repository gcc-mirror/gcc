/* Software floating-point emulation.
   Convert a _BitInt to _Decimal32.

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
extern _Decimal32 __bid_floatbitintsd (const UBILtype *, SItype);

_Decimal32
__bid_floatbitintsd (const UBILtype *i, SItype iprec)
{
  iprec = bitint_reduce_prec (&i, iprec);
  USItype aiprec = iprec < 0 ? -iprec : iprec;
  USItype in = (aiprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  USItype idx = BITINT_END (0, in - 1);
  UBILtype msb = i[idx];
  USItype mantissa;
  SItype exponent = 0;
  UBILtype inexact = 0;
  union { _Decimal32 d; USItype u; } u, ui;
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
  /* Number of bits in (_BitInt(2048)) 9999999e+90DF.  */
  if (aiprec > 323 + (iprec < 0))
    {
    ovf:
      if (iprec < 0)
	u.d = -9000000e+90DF;
      else
	u.d = 9000000e+90DF;
      __asm ("" : "+g" (u.d));
      u.d += u.d;
      __asm ("" : "+g" (u.d));
      goto done;
    }
  /* Bit precision of 9999999uwb.  */
  if (aiprec >= 24)
    {
      USItype pow10_limbs, q_limbs, q2_limbs, j;
      USItype exp_bits = 0, e;
      UDItype m;
      UBILtype *buf;
      /* First do a possibly large divide smaller enough such that
	 we only need to check remainder for 0 or non-0 and then
	 we'll do further division.  */
      if (aiprec >= 24 + 4 + 10)
	{
	  exp_bits = (aiprec - 24 - 4) / 10;
	  exponent = exp_bits * 3;
	  /* Upper estimate for pow10 (exponent) bits.  */
	  exp_bits = exp_bits * 10 - exp_bits / 30;
	}
      pow10_limbs = (exp_bits + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      /* 38 is the highest number of quotient bits needed on
	 aiprec range of [38, 323].  E.g. if aiprec is 317,
	 exponent will be 84 and exp_bits 280.  317 - 280 + 1
	 is 38.  */
      q_limbs = (38 + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      q2_limbs = (32 + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
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
      m = buf[0];
#elif BIL_TYPE_SIZE == 32
      m = ((UDItype) buf[BITINT_END (0, 1)] << 32) | buf[BITINT_END (1, 0)];
#else
# error Unsupported BIL_TYPE_SIZE
#endif
      if (m >= (UDItype) 10000000000)
	{
	  if (m >= (UDItype) 100000000000)
	    e = 5;
	  else
	    e = 4;
	}
      else if (m >= (UDItype) 100000000)
	{
	  if (m >= (UDItype) 1000000000)
	    e = 3;
	  else
	    e = 2;
	}
      else if (m >= (UDItype) 10000000)
	e = 1;
      exponent += e;
      if (exponent > 90)
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
	  mantissa = buf[q_limbs + pow10_limbs * 2 + 1];
	}
      else
#if BIL_TYPE_SIZE == 64
	mantissa = buf[0];
#else
	mantissa = buf[BITINT_END (1, 0)];
#endif
    }
  else
    {
      mantissa = msb;
      if (iprec < 0)
	mantissa = -mantissa;
    }

  exponent += 101;
  if (mantissa >= (USItype) 0x800000)
    u.u = (((((iprec < 0) << 2) | (USItype) 3) << 29)
	   | (((USItype) exponent) << 21)
	   | (mantissa ^ (USItype) 0x800000));
  else
    u.u = ((((USItype) (iprec < 0)) << 31)
	   | (((USItype) exponent) << 23)
	   | mantissa);
  if (inexact)
    {
      ui.u = ((((USItype) (iprec < 0)) << 31)
	      | (((USItype) (exponent - 1)) << 23)
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
