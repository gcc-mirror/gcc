`/* Implementation of the NEAREST intrinsic
   Copyright 2003 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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
#include <math.h>
#include <float.h>
#include "libgfortran.h"'

include(`mtype.m4')dnl

extern real_type nearest_r`'kind (real_type s, real_type dir);
export_proto(nearest_r`'kind);

real_type
nearest_r`'kind (real_type s, real_type dir)
{
  dir = copysign`'q (__builtin_inf`'q (), dir);
  if (FLT_EVAL_METHOD != 0)
    {
      /* ??? Work around glibc bug on x86.  */
      volatile real_type r = nextafter`'q (s, dir);
      return r;
    }
  else
    return nextafter`'q (s, dir);
}
