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

/* This implements IEEE 754R decimal floating point arithmetic, but
   does not provide a mechanism for setting the rounding mode, or for
   generating or handling exceptions.  Conversions between decimal
   floating point types and other types depend on C library functions.

   Contributed by Ben Elliston  <bje@au.ibm.com>.  */

/* The intended way to use this file is to make two copies, add `#define '
   to one copy, then compile both copies and add them to libgcc.a.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "config.h"
#include "gstdint.h"
#include "bid-dpd.h"
#include "decimal32.h"

uint32_t __dec_byte_swap (uint32_t);
void __host_to_ieee_32 (_Decimal32 in, decimal32 *out);
void __ieee_to_host_32 (decimal32 in, _Decimal32 *out);

#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif

uint32_t
__dec_byte_swap (uint32_t in)
{
  uint32_t out = 0;
  unsigned char *p = (unsigned char *) &out;
  union {
    uint32_t i;
    unsigned char b[4];
  } u;

  u.i = in;
  p[0] = u.b[3];
  p[1] = u.b[2];
  p[2] = u.b[1];
  p[3] = u.b[0];

  return out;
}

void
__host_to_ieee_32 (_Decimal32 in, decimal32 *out)
{
  uint32_t t;

  if (!WORDS_BIGENDIAN)
    {
      memcpy (&t, &in, 4);
      t = __dec_byte_swap (t);
      memcpy (out, &t, 4);
    }
  else
    memcpy (out, &in, 4);
}

void
__ieee_to_host_32 (decimal32 in, _Decimal32 *out)
{
  uint32_t t;

  if (!WORDS_BIGENDIAN)
    {
      memcpy (&t, &in, 4);
      t = __dec_byte_swap (t);
      memcpy (out, &t, 4);
    }
  else
    memcpy (out, &in, 4);
}
