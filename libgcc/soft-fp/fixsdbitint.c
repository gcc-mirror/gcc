/* Software floating-point emulation.
   Convert _Decimal32 to signed or unsigned _BitInt.

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
extern void __bid_fixsdbitint (UBILtype *, SItype, _Decimal32);

void
__bid_fixsdbitint (UBILtype *r, SItype rprec, _Decimal32 a)
{
  FP_DECL_EX;
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = (arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  union { _Decimal32 d; USItype u; } u;
  USItype mantissa, t;
  SItype sgn;
  SItype exponent;
  USItype exp_bits, mant_bits;
  UBILtype *pow10v, *resv;
  USItype pow10_limbs, res_limbs, min_limbs, mant_limbs, low_zeros;

  FP_INIT_EXCEPTIONS;
  u.d = a;
  t = u.u >> 21;
  sgn = (SItype) u.u < 0;
  if ((t & (3 << 8)) != (3 << 8))
    {
      mantissa = u.u & ((((USItype) 1) << 23) - 1);
      exponent = (t >> 2) & 0xff;
    }
  else if ((t & (3 << 6)) != (3 << 6))
    {
      mantissa = u.u & ((((USItype) 1) << 21) - 1);
      mantissa |= ((USItype) 1) << 23;
      exponent = t & 0xff;
      if (mantissa > (USItype) 9999999)
	mantissa = 0;
    }
  else
    {
      FP_SET_EXCEPTION (FP_EX_INVALID
			| FP_EX_INVALID_CVI
			| ((FP_EX_INVALID_SNAN
			    && ((t & 0x20)) != 0)
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
  exponent -= 101;

  if (mantissa == 0)
    {
      /* Zero (with any exponent).  */
    zero:
      __builtin_memset (r, 0, rn * sizeof (UBILtype));
      goto done;
    }
  if (exponent <= -7)
    {
      FP_SET_EXCEPTION (FP_EX_INEXACT);
      goto zero;
    }
  else if (exponent < 0)
    {
      UBILtype limbs[64 / BIL_TYPE_SIZE];
      USItype rem;
      UDItype d;
      __bid_pow10bitint (limbs, 64, -exponent);
#if BIL_TYPE_SIZE == 64
      d = limbs[0];
#elif BIL_TYPE_SIZE == 32
      d = (UDItype) limbs[BITINT_END (0, 1)] << 32 | limbs[BITINT_END (1, 0)];
#else
# error Unsupported BIL_TYPE_SIZE
#endif
      rem = mantissa % (USItype) d;
      mantissa /= (USItype) d;
      if (rem)
	FP_SET_EXCEPTION (FP_EX_INEXACT);
      if (mantissa == 0)
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
  mant_bits = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (mantissa);
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
  mant_limbs = 1;
  resv = __builtin_alloca ((res_limbs + mant_limbs) * sizeof (UBILtype));
  resv[res_limbs] = mantissa;
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
