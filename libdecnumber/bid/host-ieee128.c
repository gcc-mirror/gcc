/* This is a software decimal floating point library.
   Copyright (C) 2007-2018 Free Software Foundation, Inc.

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

#include <string.h>

#include "dconfig.h"
#include "bid-dpd.h"
#include "decimal128.h"

void __host_to_ieee_128 (_Decimal128 in, decimal128 *out);
void __ieee_to_host_128 (decimal128 in, _Decimal128 *out);

/* The code for converting 128-bit values between DPD and BID presumes
   that the 64-bit halves of the 128-bit value are in little-endian
   order, so they need swapping on big-endian hosts.  */

void
__host_to_ieee_128 (_Decimal128 in, decimal128 *out)
{
#if WORDS_BIGENDIAN
  memcpy ((char *) out, (char *) &in + 8, 8);
  memcpy ((char *) out + 8, (char *) &in, 8);
#else
  memcpy ((char *) out, (char *) &in, 16);
#endif
}

void
__ieee_to_host_128 (decimal128 in, _Decimal128 *out)
{
#if WORDS_BIGENDIAN
  memcpy ((char *) out, (char *) &in + 8, 8);
  memcpy ((char *) out + 8, (char *) &in, 8);
#else
  memcpy ((char *) out, (char *) &in, 16);
#endif
}
