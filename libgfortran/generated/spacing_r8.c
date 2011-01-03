/* Implementation of the SPACING intrinsic
   Copyright 2006, 2007, 2009, 2010 Free Software Foundation, Inc.
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

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_FREXP)

extern GFC_REAL_8 spacing_r8 (GFC_REAL_8 s, int p, int emin, GFC_REAL_8 tiny);
export_proto(spacing_r8);

GFC_REAL_8
spacing_r8 (GFC_REAL_8 s, int p, int emin, GFC_REAL_8 tiny)
{
  int e;
  if (s == 0.)
    return tiny;
  MATHFUNC(frexp) (s, &e);
  e = e - p;
  e = e > emin ? e : emin;
#if defined (HAVE_LDEXP)
  return MATHFUNC(ldexp) (1., e);
#else
  return MATHFUNC(scalbn) (1., e);
#endif
}

#endif
