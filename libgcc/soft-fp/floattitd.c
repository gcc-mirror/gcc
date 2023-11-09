/* Software floating-point emulation.
   Convert a 128bit signed integer to _Decimal128.

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
extern _Decimal128 __bid_floatbitinttd (const UBILtype *, SItype);
extern _Decimal128 __bid_floattitd (TItype);

_Decimal128
__bid_floattitd (TItype i)
{
  UBILtype ib[128 / BIL_TYPE_SIZE];
#if BIL_TYPE_SIZE == 128
  ib[0] = i;
#elif BIL_TYPE_SIZE == 64
  ib[BITINT_END (0, 1)] = i >> 64;
  ib[BITINT_END (1, 0)] = i;
#elif BIL_TYPE_SIZE == 32
  ib[BITINT_END (0, 3)] = i >> 96;
  ib[BITINT_END (1, 2)] = i >> 64;
  ib[BITINT_END (2, 1)] = i >> 32;
  ib[BITINT_END (3, 0)] = i;
#else
#error Unsupported UBILtype
#endif
  return __bid_floatbitinttd (ib, -128);
}
#endif
