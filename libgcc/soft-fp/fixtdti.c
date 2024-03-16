/* Software floating-point emulation.
   Convert _Decimal128 to 128bit signed integer.

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

#if defined(__BITINT_MAXWIDTH__) && defined(__SIZEOF_INT128__)
extern void __bid_fixtdbitint (UBILtype *, SItype, _Decimal128);
extern TItype __bid_fixtdti (_Decimal128);

TItype
__bid_fixtdti (_Decimal128 a)
{
  UBILtype rb[128 / BIL_TYPE_SIZE];
  __bid_fixtdbitint (rb, -128, a);
#if BIL_TYPE_SIZE == 128
  return rb[0];
#elif BIL_TYPE_SIZE == 64
  return ((((UTItype) rb[BITINT_END (0, 1)]) << 64)
	  | rb[BITINT_END (1, 0)]);
#elif BIL_TYPE_SIZE == 32
  return ((((UTItype) rb[BITINT_END (0, 3)]) << 96)
	  | (((UTItype) rb[BITINT_END (1, 2)]) << 64)
	  | (((UTItype) rb[BITINT_END (2, 1)]) << 32)
	  | rb[BITINT_END (3, 0)]);
#else
#error Unsupported UBILtype
#endif
}
#endif
