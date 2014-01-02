/* Implementation of the RRSPACING intrinsic
   Copyright (C) 2006-2014 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargl@gcc.gnu.org>

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



#define MATHFUNC(funcname) funcname

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_FABS) && defined (HAVE_FREXP)

extern GFC_REAL_8 rrspacing_r8 (GFC_REAL_8 s, int p);
export_proto(rrspacing_r8);

GFC_REAL_8
rrspacing_r8 (GFC_REAL_8 s, int p)
{
  int e;
  GFC_REAL_8 x;
  x = MATHFUNC(fabs) (s);
  if (x == 0.)
    return 0.;
  MATHFUNC(frexp) (s, &e);
#if defined (HAVE_LDEXP)
  return MATHFUNC(ldexp) (x, p - e);
#else
  return MATHFUNC(scalbn) (x, p - e);
#endif

}

#endif
