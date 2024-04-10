/* Software floating-point emulation.
   Convert _Decimal128 to signed or unsigned _BitInt.

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
extern void __bid_fixtdbitint (UBILtype *, SItype, _Decimal128);

void
__bid_fixtdbitint (UBILtype *r, SItype rprec, _Decimal128 a)
{
  FP_DECL_EX;
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = (arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  union { _Decimal128 d; UDItype u[2]; } u;
  UDItype mantissahi, mantissalo, t;
  SItype sgn;
  SItype exponent;
  USItype exp_bits, mant_bits;
  UBILtype *pow10v, *resv;
  USItype pow10_limbs, res_limbs, min_limbs, mant_limbs, low_zeros;

  FP_INIT_EXCEPTIONS;
  u.d = a;
  mantissahi = u.u[__FLOAT_WORD_ORDER__ != __ORDER_BIG_ENDIAN__];
  mantissalo = u.u[__FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__];
  t = mantissahi >> 47;
  sgn = (DItype) mantissahi < 0;
  if ((t & (3 << 14)) != (3 << 14))
    {
      mantissahi &= ((((UDItype) 1) << 49) - 1);
      exponent = (t >> 2) & 0x3fff;
      if (mantissahi > (UDItype) 0x1ed09bead87c0
	  || (mantissahi == (UDItype) 0x1ed09bead87c0
	      && mantissalo > (UDItype) 0x378d8e63ffffffff))
	{
	  mantissahi = 0;
	  mantissalo = 0;
	}
    }
  else if ((t & (3 << 12)) != (3 << 12))
    {
      mantissahi = 0;
      mantissalo = 0;
      exponent = t & 0x3fff;
    }
  else
    {
      FP_SET_EXCEPTION (FP_EX_INVALID
			| FP_EX_INVALID_CVI
			| ((FP_EX_INVALID_SNAN
			    && ((t & 0x800)) != 0)
			   ? FP_EX_INVALID_SNAN : 0));
    ovf:
      if (!sgn)
	__builtin_memset (r, -1, rn * sizeof (UBILtype));
      else
	__builtin_memset (r, 0, rn * sizeof (UBILtype));
      if (sgn ^ (rprec >= 0))
	r[BITINT_END (0, rn - 1)]
	  |= (UBILtype) -1 << ((arprec - 1) % BIL_TYPE_SIZE);
      else
	r[BITINT_END (0, rn - 1)]
	  &= ~((UBILtype) -1 << ((arprec - 1) % BIL_TYPE_SIZE));
      goto done;
    }
  exponent -= 6176;

  if (mantissahi == 0 && mantissalo == 0)
    {
      /* Zero (with any exponent).  */
    zero:
      __builtin_memset (r, 0, rn * sizeof (UBILtype));
      goto done;
    }
  if (exponent <= -34)
    {
      FP_SET_EXCEPTION (FP_EX_INEXACT);
      goto zero;
    }
  if (exponent < 0)
    {
      UBILtype limbs[4 * 128 / BIL_TYPE_SIZE];
#if BIL_TYPE_SIZE == 64
      limbs[BITINT_END (0, 1)] = mantissahi;
      limbs[BITINT_END (1, 0)] = mantissalo;
#elif BIL_TYPE_SIZE == 32
      limbs[BITINT_END (0, 3)] = mantissahi >> 32;
      limbs[BITINT_END (1, 2)] = mantissahi;
      limbs[BITINT_END (2, 1)] = mantissalo >> 32;
      limbs[BITINT_END (3, 0)] = mantissalo;
#elif
# error Unhandled BIL_TYPE_SIZE
#endif
      __bid_pow10bitint (&limbs[128 / BIL_TYPE_SIZE], 128, -exponent);
      __divmodbitint4 (&limbs[2 * 128 / BIL_TYPE_SIZE], 128,
		       &limbs[3 * 128 / BIL_TYPE_SIZE], 128,
		       &limbs[0], 128, &limbs[128 / BIL_TYPE_SIZE], 128);
      UDItype rem;
#if BIL_TYPE_SIZE == 64
      mantissahi = limbs[BITINT_END (4, 5)];
      mantissalo = limbs[BITINT_END (5, 4)];
      rem = limbs[6] | limbs[7];
#elif BIL_TYPE_SIZE == 32
      mantissahi = (UDItype) limbs[BITINT_END (8, 11)] << 32;
      mantissahi |= limbs[BITINT_END (9, 10)];
      mantissalo = (UDItype) limbs[BITINT_END (10, 9)] << 32;
      mantissalo |= limbs[BITINT_END (11, 8)];
      rem = limbs[12] | limbs[13] | limbs[14] | limbs[15];
#endif
      if (rem)
	FP_SET_EXCEPTION (FP_EX_INEXACT);
      if (mantissahi == 0 && mantissalo == 0)
	goto zero;
      exponent = 0;
    }

  if (rprec >= 0 && sgn)
    {
    ovf_ex:
      FP_SET_EXCEPTION (FP_EX_INVALID | FP_EX_INVALID_CVI);
      goto ovf;
    }

  /* Lower estimate for number of bits needed for pow10 (exponent).  */
  exp_bits = exponent / 3;
  exp_bits = exp_bits * 10 - exp_bits / 29;
  if (mantissahi)
    mant_bits = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (mantissahi)
		+ 64;
  else
    mant_bits = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (mantissalo);
  if (exp_bits + mant_bits > arprec + 1)
    goto ovf_ex;
  /* Upper estimate for number of bits needed for pow10 (exponent).  */
  exp_bits = (exponent + 2) / 3;
  exp_bits = exp_bits * 10 - exp_bits / 30;
  if (exp_bits == 0)
    exp_bits = 1;
  pow10_limbs = (exp_bits + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  pow10v = __builtin_alloca (pow10_limbs * sizeof (UBILtype));
  low_zeros = __bid_pow10bitint (pow10v, exp_bits, exponent);

  res_limbs = ((exp_bits + mant_bits + BIL_TYPE_SIZE - 1)
	       / BIL_TYPE_SIZE) - low_zeros;
  mant_limbs = (mant_bits + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  resv = __builtin_alloca ((res_limbs + mant_limbs) * sizeof (UBILtype));
#if BIL_TYPE_SIZE >= 64
  if (mant_limbs == 1)
    resv[res_limbs] = mantissalo;
  else
    {
      resv[res_limbs + BITINT_END (1, 0)] = mantissalo;
      resv[res_limbs + BITINT_END (0, 1)] = mantissahi;
    }
#else
  resv[res_limbs + BITINT_END (mant_limbs - 1, 0)] = mantissalo;
  if (mant_limbs >= 2)
    {
      resv[res_limbs + BITINT_END (mant_limbs - 2, 1)] = mantissalo >> 32;
      if (mant_limbs >= 3)
	{
	  resv[res_limbs + BITINT_END (mant_limbs - 3, 2)] = mantissahi;
	  if (mant_limbs == 4)
	    resv[res_limbs + BITINT_END (0, 3)] = mantissahi >> 32;
	}
    }
#endif
  __mulbitint3 (resv, exp_bits + mant_bits - low_zeros * BIL_TYPE_SIZE,
		resv + res_limbs, mant_bits,
		pow10v + BITINT_END (0, low_zeros),
		exp_bits - low_zeros * BIL_TYPE_SIZE);
  if (res_limbs + low_zeros >= rn)
    {
      if (res_limbs + low_zeros > rn && resv[BITINT_END (0, res_limbs - 1)])
	goto ovf_ex;
      if ((arprec % BIL_TYPE_SIZE) != 0
	  && (resv[BITINT_END (rn - res_limbs, rn - 1) - low_zeros]
	      & ((UBILtype) -1 << (arprec % BIL_TYPE_SIZE))) != 0)
	goto ovf_ex;
      min_limbs = rn - low_zeros;
    }
  else
    min_limbs = res_limbs;
  if (low_zeros)
    __builtin_memset (r + BITINT_END (rn - low_zeros, 0), '\0',
		      low_zeros * sizeof (UBILtype));
  if (sgn)
    bitint_negate (r + BITINT_END (rn - low_zeros - 1, low_zeros),
		   resv + BITINT_END (res_limbs - 1, 0), min_limbs);
  else
    __builtin_memcpy (r + BITINT_END (rn - low_zeros - min_limbs, low_zeros),
		      resv + BITINT_END (res_limbs - min_limbs, 0),
		      min_limbs * sizeof (UBILtype));
  if (res_limbs + low_zeros < rn)
    {
      if (sgn)
	__builtin_memset (r + BITINT_END (0, res_limbs + low_zeros), -1,
			  (rn - res_limbs - low_zeros) * sizeof (UBILtype));
      else
	__builtin_memset (r + BITINT_END (0, res_limbs + low_zeros), '\0',
			  (rn - res_limbs - low_zeros) * sizeof (UBILtype));
    }
  else if (sgn)
    {
      if ((r[BITINT_END (0, rn - 1)]
	   & ((UBILtype) 1 << ((arprec - 1) % BIL_TYPE_SIZE))) == 0)
	goto ovf_ex;
    }
  else if (rprec < 0
	   && (r[BITINT_END (0, rn - 1)]
	       & ((UBILtype) 1 << ((arprec - 1) % BIL_TYPE_SIZE))) != 0)
    goto ovf_ex;

done:
  FP_HANDLE_EXCEPTIONS;
}
#endif
