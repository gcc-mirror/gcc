/* Software floating-point emulation.
   Convert IBM double double to signed or unsigned _BitInt.

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
void
__fixtfbitint (UBILtype *r, SItype rprec, __ibm128 a)
{
  FP_DECL_EX;
  FP_DECL_D (A);
  FP_DECL_D (B);
  union { __ibm128 a; DFtype b[2]; } ua = { .a = a };
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = ((USItype) arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  UDItype rv;
  USItype rsize = arprec > DI_BITS ? DI_BITS : arprec;
  USItype rsigned = rprec < 0;
  USItype ovf = 0;
  USItype shift = 0;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, ua.b[0]);
  if (arprec > DI_BITS)
    {
      if (A_e < _FP_EXPBIAS_D || (A_s && !rsigned))
	ovf = 1;
      else if (A_e >= (_FP_EXPMAX_D < _FP_EXPBIAS_D + arprec
		       ? _FP_EXPMAX_D
		       : _FP_EXPBIAS_D + arprec - rsigned))
	{
	  ovf = 1;
	  if (A_s
	      && A_e == _FP_EXPBIAS_D + arprec - 1
	      && A_e < _FP_EXPMAX_D)
	    A_e -= arprec - DI_BITS;
	}
      else if (A_e >= _FP_EXPBIAS_D + DI_BITS - rsigned)
	{
	  shift = A_e - (_FP_EXPBIAS_D + DI_BITS - rsigned - 1);
	  A_e -= shift;
	}
    }
  FP_TO_INT_D (rv, A, rsize, rsigned);
  FP_TO_BITINT (r, rn, arprec, shift, rv, rsize, rsigned, ovf, DI);
  if (!ovf)
    {
      FP_UNPACK_RAW_D (B, ua.b[1]);
      /* For valid __ibm128, B_e < A_e except for the NaN and +-0
	 cases, but those are all ovf = 1.  Always treat B as
	 signed.  */
      shift = 0;
      rsize = arprec > DI_BITS ? DI_BITS : arprec;
      if (arprec > DI_BITS)
	{
	  if (B_e < _FP_EXPBIAS_D)
	    ovf = 1;
	  else if (B_e >= _FP_EXPBIAS_D + DI_BITS - 1)
	    {
	      shift = B_e - (_FP_EXPBIAS_D + DI_BITS - 2);
	      B_e -= shift;
	    }
	}
      FP_TO_INT_D (rv, B, rsize, 1);
      if (!ovf)
	{
	  /* Efficiently add
	     ((_BitInt(arprec)) (DItype) rv) << shift to
	     *((_BitInt(arprec) *) r) resp.
	     *((unsigned _BitInt(arprec) *) r).  */
	  USItype shiftl = shift / BIL_TYPE_SIZE;
	  rsize = DI_BITS;
	  if (shift + DI_BITS > arprec)
	    rsize = arprec - shift;
	  USItype shiftr = shift % BIL_TYPE_SIZE;
	  USItype idx = BITINT_END (rn - shiftl - 1, shiftl);
	  DItype rvs = rv;
	  UBILtype c = 0;
	  if (shiftr)
	    {
	      c = __builtin_add_overflow (r[idx], ((UBILtype) rvs) << shiftr,
					  &r[idx]);
	      idx += BITINT_INC;
	      if (rsize > BIL_TYPE_SIZE - shiftr)
		{
		  rvs >>= BIL_TYPE_SIZE - shiftr;
		  rsize -= BIL_TYPE_SIZE - shiftr;
		}
	      else
		rsize = 0;
	    }
	  while (rsize)
	    {
	      UBILtype c2
		= __builtin_add_overflow (r[idx], (UBILtype) rvs, &r[idx]);
	      c = __builtin_add_overflow (r[idx], c, &r[idx]) | c2;
	      idx += BITINT_INC;
	      if (rsize <= BIL_TYPE_SIZE)
		break;
	      rvs >>= (DI_BITS > BIL_TYPE_SIZE ? BIL_TYPE_SIZE : 0);
	      rsize -= BIL_TYPE_SIZE;
	    }
	  if (rvs < 0)
	    while (!c && idx < rn)
	      {
		/* If rvs is negative, we want to add -1 + c
		   to the limb.  If c is 0, we can stop, as nothing
		   changes.  */
		c = __builtin_add_overflow (r[idx], (UBILtype) -1, &r[idx]);
		idx += BITINT_INC;
	      }
	  else
	    while (c && idx < rn)
	      {
		c = __builtin_add_overflow (r[idx], (UBILtype) 1, &r[idx]);
		idx += BITINT_INC;
	      }
	}
    }
  FP_HANDLE_EXCEPTIONS;
}
#endif
