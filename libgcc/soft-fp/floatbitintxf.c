/* Software floating-point emulation.
   Convert a _BitInt to IEEE extended.

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
#include "extended.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
#ifndef TI_BITS
/* While mantissa is 64 bits including 1 explicit bit, extended.h uses
   op-2.h for W_TYPE_SIZE 64 and op-4.h for W_TYPE_SIZE 32, so we have
   to use 128-bit type here.  On most 32-bit architectures TImode isn't
   supported, so use _BitInt(128) instead.  */
typedef _BitInt(128) TItype;
typedef unsigned _BitInt(128) UTItype;
#define TI_BITS 128
#endif

XFtype
__floatbitintxf (const UBILtype *i, SItype iprec)
{
  TItype iv;
  USItype shift = 0;
  FP_DECL_EX;
  FP_DECL_E (A);
  XFtype a;

  FP_FROM_BITINT (i, iprec, iv, shift, TI);
  FP_INIT_ROUNDMODE;
  FP_FROM_INT_E (A, iv, TI_BITS, UTItype);
  if (shift)
    {
      A_e += shift;
      if (A_e >= _FP_EXPMAX_E)
	{
	  /* Exponent too big; overflow to infinity.  */
#if _FP_W_TYPE_SIZE < 64
	  _FP_OVERFLOW_SEMIRAW (E, 4, A);
	  _FP_PACK_SEMIRAW (E, 4, A);
#else
	  _FP_OVERFLOW_SEMIRAW (E, 2, A);
	  _FP_PACK_SEMIRAW (E, 2, A);
#endif
	}
    }
  FP_PACK_RAW_E (a, A);
  FP_HANDLE_EXCEPTIONS;

  return a;
}
#endif
