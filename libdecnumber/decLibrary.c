/* Temporary library support for decimal floating point.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "dconfig.h"
#include "decContext.h"
#include "decimal128.h"
#include "decimal64.h"
#include "decimal32.h"

void __host_to_ieee_32 (_Decimal32, decimal32 *);
void __host_to_ieee_64 (_Decimal64, decimal64 *);
void __host_to_ieee_128 (_Decimal128, decimal128 *);

extern int isinfd32 (_Decimal32);
extern int isinfd64 (_Decimal64);
extern int isinfd128 (_Decimal128);
uint32_t __dec_byte_swap (uint32_t);

int
isinfd32 (_Decimal32 arg)
{
  decNumber dn;
  decimal32 d32;

  __host_to_ieee_32 (arg, &d32);
  decimal32ToNumber (&d32, &dn);
  return (decNumberIsInfinite (&dn));
}

int
isinfd64 (_Decimal64 arg)
{
  decNumber dn;
  decimal64 d64;

  __host_to_ieee_64 (arg, &d64);
  decimal64ToNumber (&d64, &dn);
  return (decNumberIsInfinite (&dn));
}

int
isinfd128 (_Decimal128 arg)
{
  decNumber dn;
  decimal128 d128;

  __host_to_ieee_128 (arg, &d128);
  decimal128ToNumber (&d128, &dn);
  return (decNumberIsInfinite (&dn));
}
