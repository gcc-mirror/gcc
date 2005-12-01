/* Temporary library support for decimal floating point.
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
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
extern void __dfp_enable_traps (void);
extern void __dfp_raise (int exception __attribute__ ((unused)));

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

int __dfp_traps;

void
__dfp_enable_traps (void)
{
  __dfp_traps = 1;
}

void
__dfp_raise (int exception __attribute__ ((unused)))
{
  raise (SIGFPE);
}

unsigned long
__dec_byte_swap (unsigned long in)
{
  unsigned long out;
  unsigned char *p = (unsigned char *) &out;
  union {
    unsigned long i;
    unsigned char b[4];
  } u;

  u.i = in;
  p[0] = u.b[3];
  p[1] = u.b[2];
  p[2] = u.b[1];
  p[3] = u.b[0];

  return out;
}
