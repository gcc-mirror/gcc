/* Software floating-point emulation.
   Convert a _BitInt to IEEE half.

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
#include "half.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
HFtype
__floatbitinthf (const UBILtype *i, SItype iprec)
{
  SItype iv;
  USItype shift = 0;
  FP_DECL_EX;
  FP_DECL_H (A);
  HFtype a;

  FP_FROM_BITINT (i, iprec, iv, shift, SI);
  FP_INIT_ROUNDMODE;
  FP_FROM_INT_H (A, iv, SI_BITS, USItype);
  if (shift)
    {
      A_e += shift;
      if (A_e >= _FP_EXPMAX_H)
	{
	  /* Exponent too big; overflow to infinity.  */
	  _FP_OVERFLOW_SEMIRAW (H, 1, A);
	  _FP_PACK_SEMIRAW (H, 1, A);
	}
    }
  FP_PACK_RAW_H (a, A);
  FP_HANDLE_EXCEPTIONS;

  return a;
}
#endif
