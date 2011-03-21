/* Implementation of the NEAREST intrinsic
   Copyright 2003, 2007, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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



#if defined(GFC_REAL_16_IS_FLOAT128)
#define MATHFUNC(funcname) funcname ## q
#else
#define MATHFUNC(funcname) funcname ## l
#endif

#if defined (HAVE_GFC_REAL_16) && (defined(GFC_REAL_16_IS_FLOAT128) || defined(HAVE_COPYSIGNL)) && (defined(GFC_REAL_16_IS_FLOAT128) || defined(HAVE_NEXTAFTERL))

extern GFC_REAL_16 nearest_r16 (GFC_REAL_16 s, GFC_REAL_16 dir);
export_proto(nearest_r16);

GFC_REAL_16
nearest_r16 (GFC_REAL_16 s, GFC_REAL_16 dir)
{
  dir = MATHFUNC(copysign) (MATHFUNC(__builtin_inf) (), dir);
  if (FLT_EVAL_METHOD != 0)
    {
      /* ??? Work around glibc bug on x86.  */
      volatile GFC_REAL_16 r = MATHFUNC(nextafter) (s, dir);
      return r;
    }
  else
    return MATHFUNC(nextafter) (s, dir);
}

#endif
