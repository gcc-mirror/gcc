/* This is a software decimal floating point library.
   Copyright (C) 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

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
