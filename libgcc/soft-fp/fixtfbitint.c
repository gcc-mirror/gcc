/* Software floating-point emulation.
   Convert IEEE quad to signed or unsigned _BitInt.

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
#include "quad.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__

#ifndef TI_BITS
/* As mantissa is 112 bits + 1 implicit bit, we need 128-bit
   type, but on most 32-bit architectures TImode isn't supported.
   Use _BitInt(128) instead.  */
typedef _BitInt(128) TItype;
typedef unsigned _BitInt(128) UTItype;
#define TI_BITS 128
#endif

void
__fixtfbitint (UBILtype *r, SItype rprec, TFtype a)
{
  FP_DECL_EX;
  FP_DECL_Q (A);
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = ((USItype) arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  UTItype rv;
  USItype rsize = arprec > TI_BITS ? TI_BITS : arprec;
  USItype rsigned = rprec < 0;
  USItype ovf = 0;
  USItype shift = 0;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_Q (A, a);
  if (arprec > TI_BITS)
    {
      if (A_e < _FP_EXPBIAS_Q || (A_s && !rsigned))
	ovf = 1;
      else if (A_e >= (_FP_EXPMAX_Q < _FP_EXPBIAS_Q + arprec
		       ? _FP_EXPMAX_Q
		       : _FP_EXPBIAS_Q + arprec - rsigned))
	{
	  ovf = 1;
	  if (A_s
	      && A_e == _FP_EXPBIAS_Q + arprec - 1
	      && A_e < _FP_EXPMAX_Q)
	    A_e -= arprec - TI_BITS;
	}
      else if (A_e >= _FP_EXPBIAS_Q + TI_BITS - rsigned)
	{
	  shift = A_e - (_FP_EXPBIAS_Q + TI_BITS - rsigned - 1);
	  A_e -= shift;
	}
    }
  FP_TO_INT_Q (rv, A, rsize, rsigned);
  FP_HANDLE_EXCEPTIONS;
  FP_TO_BITINT (r, rn, arprec, shift, rv, rsize, rsigned, ovf, TI);
}
#endif
