/* Implementation of the bit intrinsics not implemented as GCC builtins.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"


#ifdef HAVE_GFC_INTEGER_16
extern int clz128 (GFC_INTEGER_16);
export_proto(clz128);

int
clz128 (GFC_INTEGER_16 x)
{
  int res = 127;

  // We can't write 0xFFFFFFFFFFFFFFFF0000000000000000, so we work around it
  if (x & ((__uint128_t) 0xFFFFFFFFFFFFFFFF << 64))
    {
      res -= 64;
      x >>= 64;
    }

  if (x & 0xFFFFFFFF00000000)
    {
      res -= 32;
      x >>= 32;
    }

  if (x & 0xFFFF0000)
    {
      res -= 16;
      x >>= 16;
    }

  if (x & 0xFF00)
    {
      res -= 8;
      x >>= 8;
    }

  if (x & 0xF0)
    {
      res -= 4;
      x >>= 4;
    }

  if (x & 0xC)
    {
      res -= 2;
      x >>= 2;
    }

  if (x & 0x2)
    {
      res -= 1;
      x >>= 1;
    }

  return res;
}
#endif


#ifdef HAVE_GFC_INTEGER_16
extern int ctz128 (GFC_INTEGER_16);
export_proto(ctz128);

int
ctz128 (GFC_INTEGER_16 x)
{
  int res = 0;

  if ((x & 0xFFFFFFFFFFFFFFFF) == 0)
    {
      res += 64;
      x >>= 64;
    }

  if ((x & 0xFFFFFFFF) == 0)
    {
      res += 32;
      x >>= 32;
    }

  if ((x & 0xFFFF) == 0)
    {
      res += 16;
      x >>= 16;
    }

  if ((x & 0xFF) == 0)
    {
      res += 8;
      x >>= 8;
    }

  if ((x & 0xF) == 0)
    {
      res += 4;
      x >>= 4;
    }

  if ((x & 0x3) == 0)
    {
      res += 2;
      x >>= 2;
    }

  if ((x & 0x1) == 0)
    {
      res += 1;
      x >>= 1;
    }

  return res;
}
#endif
