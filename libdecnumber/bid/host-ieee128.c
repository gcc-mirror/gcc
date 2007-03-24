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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "config.h"
#include "gstdint.h"
#include "bid-dpd.h"
#include "decimal128.h"

extern uint32_t __dec_byte_swap (uint32_t);
void __host_to_ieee_128 (_Decimal128 in, decimal128 *out);
void __ieee_to_host_128 (decimal128 in, _Decimal128 *out);

#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif

static void
__swap128 (char *src, char *dst)
{
  uint32_t t1, t2, t3, t4;

  if (!WORDS_BIGENDIAN)
    {
      memcpy (&t1, src, 4);
      memcpy (&t2, src + 4, 4);
      memcpy (&t3, src + 8, 4);
      memcpy (&t4, src + 12, 4);
      t1 = __dec_byte_swap (t1);
      t2 = __dec_byte_swap (t2);
      t3 = __dec_byte_swap (t3);
      t4 = __dec_byte_swap (t4);
      memcpy (dst, &t4, 4);
      memcpy (dst + 4, &t3, 4);
      memcpy (dst + 8, &t2, 4);
      memcpy (dst + 12, &t1, 4);
    }
  else
    memcpy (dst, src, 16);
}

void
__host_to_ieee_128 (_Decimal128 in, decimal128 *out)
{
  __swap128 ((char *) &in, (char *) out);
}

void
__ieee_to_host_128 (decimal128 in, _Decimal128 *out)
{
  __swap128 ((char *) &in, (char *) out);
}
