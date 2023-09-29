/* Software floating-point emulation.
   Convert IEEE single to signed or unsigned _BitInt.

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
#include "single.h"
#include "bitint.h"

#ifdef __BITINT_MAXWIDTH__
void
__fixsfbitint (UBILtype *r, SItype rprec, SFtype a)
{
  FP_DECL_EX;
  FP_DECL_S (A);
  USItype arprec = rprec < 0 ? -rprec : rprec;
  USItype rn = ((USItype) arprec + BIL_TYPE_SIZE - 1) / BIL_TYPE_SIZE;
  USItype rv;
  USItype rsize = arprec > SI_BITS ? SI_BITS : arprec;
  USItype rsigned = rprec < 0;
  USItype ovf = 0;
  USItype shift = 0;

  FP_INIT_EXCEPTIONS;
  FP_UNPACK_RAW_S (A, a);
  if (arprec > SI_BITS)
    {
      if (A_e < _FP_EXPBIAS_S || (A_s && !rsigned))
	ovf = 1;
      else if (A_e >= (_FP_EXPMAX_S < _FP_EXPBIAS_S + arprec
		       ? _FP_EXPMAX_S
		       : _FP_EXPBIAS_S + arprec - rsigned))
	{
	  ovf = 1;
	  if (A_s
	      && A_e == _FP_EXPBIAS_S + arprec - 1
	      && A_e < _FP_EXPMAX_S)
	    A_e -= arprec - SI_BITS;
	}
      else if (A_e >= _FP_EXPBIAS_S + SI_BITS - rsigned)
	{
	  shift = A_e - (_FP_EXPBIAS_S + SI_BITS - rsigned - 1);
	  A_e -= shift;
	}
    }
  FP_TO_INT_S (rv, A, rsize, rsigned);
  FP_HANDLE_EXCEPTIONS;
  FP_TO_BITINT (r, rn, arprec, shift, rv, rsize, rsigned, ovf, SI);
}
#endif
