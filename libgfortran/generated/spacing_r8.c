/* Implementation of the SPACING intrinsic
   Copyright 2006, 2007 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargl@gcc.gnu.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"


#if defined (HAVE_GFC_REAL_8) && defined (HAVE_FREXP)

extern GFC_REAL_8 spacing_r8 (GFC_REAL_8 s, int p, int emin, GFC_REAL_8 tiny);
export_proto(spacing_r8);

GFC_REAL_8
spacing_r8 (GFC_REAL_8 s, int p, int emin, GFC_REAL_8 tiny)
{
  int e;
  if (s == 0.)
    return tiny;
  frexp (s, &e);
  e = e - p;
  e = e > emin ? e : emin;
#if defined (HAVE_LDEXP)
  return ldexp (1., e);
#else
  return scalbn (1., e);
#endif
}

#endif
