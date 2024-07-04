/* Software floating-point emulation.
   Convert a _BitInt to _Decimal128.

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
extern _Decimal128 __bid_floatbitinttd (const UBILtype *, SItype);

_Decimal128
__bid_floatbitinttd (const UBILtype *i, SItype iprec)
{
  iprec = bitint_reduce_prec (&i, iprec);
  USItype aiprec = iprec < 0 ? -iprec : iprec;
  USItype in = (aiprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  USItype idx = BITINT_END (0, in - 1);
  UBILtype msb = i[idx];
  UDItype mantissahi, mantissalo;
  SItype exponent = 0;
  UBILtype inexact = 0;
  union { _Decimal128 d; UDItype u[2]; } u, ui;
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
  /* Number of bits in
     (_BitInt(32768)) 9999999999999999999999999999999999e+6111DL.  */
  if (aiprec > 20414 + (iprec < 0))
    {
    ovf:
      if (iprec < 0)
	u.d = -9000000000000000000000000000000000e+6111DL;
      else
	u.d = 9000000000000000000000000000000000e+6111DL;
      __asm ("" : "+g" (u.d));
      u.d += u.d;
      __asm ("" : "+g" (u.d));
      goto done;
    }
  /* Bit precision of 9999999999999999999999999999999999uwb.  */
  if (aiprec >= 113)
    {
      USItype pow10_limbs, q_limbs, q2_limbs, j, k;
      USItype exp_bits = 0, e;
      UBILtype *buf;
      /* First do a possibly large divide smaller enough such that
	 we only need to check remainder for 0 or non-0 and then
	 we'll do further division.  */
      if (aiprec >= 113 + 4 + 10)
	{
	  exp_bits = ((aiprec - 113 - 4) * (UDItype) 30) / 299;
	  exponent = exp_bits * 3;
	  /* Upper estimate for pow10 (exponent) bits.  */
	  exp_bits = exp_bits * 10 - exp_bits / 30;
	}
      pow10_limbs = (exp_bits + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      /* 127 is the highest number of quotient bits needed on
	 aiprec range of [127, 20414].  E.g. if aiprec is 20409,
	 exponent will be 6105 and exp_bits 20283.  20409 - 20283 + 1
	 is 127.  */
      q_limbs = (127 + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      q2_limbs = 128 / BIL_TYPE_SIZE;
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
      for (j = 3; j; )
	{
	  USItype eprev = e;
	  __bid_pow10bitint (buf + q_limbs + pow10_limbs * 2 + 1,
			     128, 33 + e + j);
	  for (k = BITINT_END (0, q_limbs - 1);
	       k != BITINT_END (q_limbs, (USItype) -1); k -= BITINT_INC)
	    if (buf[k] > buf[q_limbs + pow10_limbs * 2 + 1 + k])
	      {
		e += j;
		break;
	      }
	    else if (buf[k] < buf[q_limbs + pow10_limbs * 2 + 1 + k])
	      break;
	  if (k == BITINT_END (q_limbs, (USItype) -1))
	    e += j;
	  if (j == 2 && e != eprev)
	    break;
	  else
	    --j;
	}
      exponent += e;
      if (exponent > 6111)
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
	  mantissahi = buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (0, 1)];
	  mantissalo = buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (1, 0)];
#else
	  mantissahi
	    = ((UDItype)
	       buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (0, 3)] << 32
	       | buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (1, 2)]);
	  mantissalo
	    = ((UDItype)
	       buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (2, 1)] << 32
	       | buf[q_limbs + pow10_limbs * 2 + 1 + BITINT_END (3, 0)]);
#endif
	}
      else
	{
#if BIL_TYPE_SIZE == 64
	  mantissahi = buf[BITINT_END (0, 1)];
	  mantissalo = buf[BITINT_END (1, 0)];
#else
	  mantissahi = ((UDItype) buf[BITINT_END (0, 3)] << 32
			| buf[BITINT_END (1, 2)]);
	  mantissalo = ((UDItype) buf[BITINT_END (2, 1)] << 32
			| buf[BITINT_END (3, 0)]);
#endif
	}
    }
  else
    {
      mantissahi = iprec < 0 ? -1 : 0;
#if BIL_TYPE_SIZE == 64
      if (in == 1)
	mantissalo = msb;
      else
	{
	  mantissahi = msb;
	  mantissalo = i[BITINT_END (1, 0)];
	}
#else
      if (in <= 2)
	{
	  if (in == 1)
	    mantissalo = iprec < 0 ? (UDItype) (BILtype) msb : (UDItype) msb;
	  else
	    mantissalo = (UDItype) msb << 32 | i[BITINT_END (1, 0)];
	}
      else
	{
	  if (in == 3)
	    mantissahi = iprec < 0 ? (UDItype) (BILtype) msb : (UDItype) msb;
	  else
	    mantissahi = (UDItype) msb << 32 | i[BITINT_END (1, 2)];
	  mantissalo = ((UDItype) i[BITINT_END (in - 2, 1)] << 32
			| i[BITINT_END (in - 1, 0)]);
	}
#endif
      if (iprec < 0)
	mantissahi
	  = ~mantissahi + __builtin_add_overflow (~mantissalo, 1, &mantissalo);
    }

  exponent += 6176;
  u.u[__FLOAT_WORD_ORDER__ != __ORDER_BIG_ENDIAN__]
    = ((((UDItype) (iprec < 0)) << 63)
       | (((UDItype) exponent) << 49)
       | mantissahi);
  u.u[__FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__] = mantissalo;
  if (inexact)
    {
      ui.u[__FLOAT_WORD_ORDER__ != __ORDER_BIG_ENDIAN__]
	= (((UDItype) (iprec < 0)) << 63) | (((UDItype) exponent - 1) << 49);
      ui.u[__FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__] = inexact + 3;
      __asm ("" : "+g" (u.d));
      __asm ("" : "+g" (ui.d));
      u.d += ui.d;
      __asm ("" : "+g" (u.d));
    }

done:
  return u.d;
}
#endif
