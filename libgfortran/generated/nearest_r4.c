/* Implementation of the NEAREST intrinsic
   Copyright 2003 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Ligbfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
#include <math.h>
#include <float.h>
#include "libgfortran.h"


GFC_REAL_4
prefix(nearest_r4) (GFC_REAL_4 s, GFC_REAL_4 dir)
{
  dir = copysignf (__builtin_inff (), dir);
  if (FLT_EVAL_METHOD != 0)
    {
      /* ??? Work around glibc bug on x86.  */
      volatile GFC_REAL_4 r = nextafterf (s, dir);
      return r;
    }
  else
    return nextafterf (s, dir);
}
