/* Software floating-point emulation.
   Convert a _BitInt to IBM double double.

   Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include "double.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
__ibm128
__floatbitinttf (const UBILtype *i, SItype iprec)
{
  DItype iv;
  USItype shift = 0;
  FP_DECL_EX;
  FP_DECL_D (A);
  FP_DECL_D (B);
  union { __ibm128 a; DFtype b[2]; } ua;

  FP_FROM_BITINT (i, iprec, iv, shift, DI);
  FP_INIT_ROUNDMODE;
  FP_FROM_INT_D (A, iv, DI_BITS, UDItype);
  if (shift)
    {
      A_e += shift;
      if (A_e >= _FP_EXPMAX_D)
	{
	  /* Exponent too big; overflow to infinity.  */
#if _FP_W_TYPE_SIZE < 64
	  _FP_OVERFLOW_SEMIRAW (D, 2, A);
	  _FP_PACK_SEMIRAW (D, 2, A);
#else
	  _FP_OVERFLOW_SEMIRAW (D, 1, A);
	  _FP_PACK_SEMIRAW (D, 1, A);
#endif
	}
    }
  FP_PACK_RAW_D (ua.b[0], A);
  if (A_e >= _FP_EXPMAX_D || !iv)
    ua.b[1] = 0;
  else
    {
      USItype shift2 = 0;
      UDItype iv2;
      if (A_e >= _FP_EXPBIAS_D + _FP_FRACBITS_D + 1)
	{
	  shift2 = A_e - (_FP_EXPBIAS_D + _FP_FRACBITS_D);
	  A_e -= shift2;
	}
      FP_TO_INT_D (iv2, A, DI_BITS, 1);
      USItype shiftl = shift / BIL_TYPE_SIZE;
      USItype shiftr = shift % BIL_TYPE_SIZE;
      USItype aiprec = iprec < 0 ? -iprec : iprec;
      USItype in = (aiprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
      (void) in;
      USItype idx = BITINT_END (in - shiftl - 1, shiftl);
      /* The low bit from FP_FROM_BITINT has all lower bits ored into it.
	 Undo this now.  */
      if (((i[idx] >> shiftr) & 1) == 0)
	iv &= ~(UDItype) 1;
      iv = iv - (iv2 << (shift2 - shift));
      /* A has been rounded down by
	 ((_BitInt(iprec < 0 ? -iprec : iprec)) (DItype) iv) << shift.
	 Next step is to perform FP_FROM_BITINT virtually on a value
	 that has bits from iv for bits shift and up and bits from
	 i for the lower bits.  */
      if (shift)
	{
	  SItype n = DI_BITS + 1;
	  if (iv < 0)
	    {
	      if (iv != -1)
		{
		  n = (sizeof (0ULL) * __CHAR_BIT__ + 1
		       - __builtin_clzll (~iv));
		  n = DI_BITS - n;
		}
	    }
	  else if (iv)
	    {
	      n = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (iv);
	      n = DI_BITS - 1 - n;
	    }
	  shift = 0;
	  UBILtype msb = 0;
	  if (shiftr != 0)
	    {
	      msb = i[idx] & (((UBILtype) 1 << shiftr) - 1);
	      if (iv == 0)
		{
		  if (msb != 0)
		    goto first_nonzero;
		}
	      else if (iv == -1)
		{
		  if (msb != ((UBILtype) 1 << shiftr) - 1)
		    {
		      msb |= (UBILtype) -1 << shiftr;
		      goto first_nonminusone;
		    }
		}
	      else if (shiftr < DI_BITS && n >= (SItype) shiftr)
		{
		  iv = (UDItype) iv << (shiftr < DI_BITS ? shiftr : 0);
		  iv |= msb;
		  n -= shiftr;
		}
	      else
		{
		  iv = (UDItype) iv << n;
		  iv |= msb >> (shiftr - n);
		  shift = shiftr - n;
		  n = 0;
		}
	    }
	  if (iv == 0 && n == DI_BITS + 1)
	    while (BITINT_END (idx < in - 1, idx))
	      {
		idx -= BITINT_INC;
		msb = i[idx];
		if (msb != 0)
		  {
		    if ((BILtype) msb < 0)
		      {
			idx += BITINT_INC;
			n = DI_BITS - 1;
			break;
		      }
		  first_nonzero:
		    n = sizeof (0ULL) * __CHAR_BIT__ - __builtin_clzll (msb);
		    if (BIL_TYPE_SIZE >= DI_BITS && n >= DI_BITS)
		      {
			iv = msb >> (n - DI_BITS + 1);
			shift = n - DI_BITS + 1;
			n = 0;
		      }
		    else
		      {
			iv = msb;
			n = DI_BITS - 1 - n;
		      }
		    break;
		  }
	      }
	  if (iv == -1 && n == DI_BITS + 1)
	    while (BITINT_END (idx < in - 1, idx))
	      {
		idx -= BITINT_INC;
		msb = i[idx];
		if (msb != (UBILtype) -1)
		  {
		    if ((BILtype) msb >= 0)
		      {
			idx += BITINT_INC;
			n = DI_BITS - 1;
			break;
		      }
		  first_nonminusone:
		    n = (sizeof (0ULL) * __CHAR_BIT__ + 1
			 - __builtin_clzll (~msb));
		    if (BIL_TYPE_SIZE > DI_BITS && n > DI_BITS)
		      {
			iv = msb >> (n - DI_BITS);
			shift = n - DI_BITS;
			n = 0;
		      }
		    else
		      {
			iv = (BILtype) msb;
			n = DI_BITS - n;
		      }
		    break;
		  }
	      }
	  while (n && BITINT_END (idx < in - 1, idx))
	    {
	      idx -= BITINT_INC;
	      msb = i[idx];
	      if (BIL_TYPE_SIZE < DI_BITS && n >= BIL_TYPE_SIZE)
		{
		  iv = (UDItype) iv << (BIL_TYPE_SIZE < DI_BITS
					? BIL_TYPE_SIZE : 0);
		  iv |= msb;
		  n -= BIL_TYPE_SIZE;
		}
	      else
		{
		  iv = (UDItype) iv << n;
		  iv |= msb >> (BIL_TYPE_SIZE - n);
		  shift = BIL_TYPE_SIZE - n;
		  break;
		}
	    }

	  UBILtype low_bits = 0;
	  if (shift)
	    low_bits = msb & (((UBILtype) 1 << shift) - 1);
	  shift += BITINT_END (in - 1 - idx, idx) * BIL_TYPE_SIZE;
	  while (!low_bits && BITINT_END (idx < in - 1, idx))
	    {
	      idx -= BITINT_INC;
	      low_bits |= i[idx];
	    }
	  iv |= (low_bits != 0);
	}
      FP_FROM_INT_D (B, iv, DI_BITS, UDItype);
      if (shift)
	B_e += shift;
      FP_PACK_RAW_D (ua.b[1], B);
    }
  FP_HANDLE_EXCEPTIONS;

  return ua.a;
}
#endif
