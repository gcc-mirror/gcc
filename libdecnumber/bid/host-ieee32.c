/* This is a software decimal floating point library.
   Copyright (C) 2007-2014 Free Software Foundation, Inc.

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

/* This implements IEEE 754R decimal floating point arithmetic, but
   does not provide a mechanism for setting the rounding mode, or for
   generating or handling exceptions.  Conversions between decimal
   floating point types and other types depend on C library functions.

   Contributed by Ben Elliston  <bje@au.ibm.com>.  */

/* The intended way to use this file is to make two copies, add `#define '
   to one copy, then compile both copies and add them to libgcc.a.  */

#include <string.h>
#include "bid-dpd.h"
#include "decimal32.h"

void __host_to_ieee_32 (_Decimal32 in, decimal32 *out);
void __ieee_to_host_32 (decimal32 in, _Decimal32 *out);

void
__host_to_ieee_32 (_Decimal32 in, decimal32 *out)
{
  memcpy ((char *) out, (char *) &in, 4);
}

void
__ieee_to_host_32 (decimal32 in, _Decimal32 *out)
{
  memcpy ((char *) out, (char *) &in, 4);
}
