/* Software floating-point emulation.
   Convert IEEE double to signed or unsigned _BitInt.

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
#include "double.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
void
__fixdfbitint (UBILtype *r, SItype rprec, DFtype a)
{
  FP_DECL_EX;
  FP_DECL_D (A);
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = ((USItype) arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  UDItype rv;
  USItype rsize = arprec > DI_BITS ? DI_BITS : arprec;
  USItype rsigned = rprec < 0;
  USItype ovf = 0;
  USItype shift = 0;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_D (A, a);
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
  FP_HANDLE_EXCEPTIONS;
  FP_TO_BITINT (r, rn, arprec, shift, rv, rsize, rsigned, ovf, DI);
}
#endif
