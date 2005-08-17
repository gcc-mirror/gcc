/* Complex hyperbolic functions
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
#include "libgfortran.h"


/* Complex number z = a + ib.  */

/* sinh(z) = sinh(a)cos(b) + icosh(a)sin(b)  */
GFC_COMPLEX_8
csinh (GFC_COMPLEX_8 a)
{
  GFC_REAL_8 r;
  GFC_REAL_8 i;
  GFC_COMPLEX_8 v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinh (r) * cos (i), cosh (r) * sin (i));
  return v;
}

/* cosh(z) = cosh(a)cos(b) - isinh(a)sin(b)  */
GFC_COMPLEX_8
ccosh (GFC_COMPLEX_8 a)
{
  GFC_REAL_8 r;
  GFC_REAL_8 i;
  GFC_COMPLEX_8 v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cosh (r) * cos (i), - (sinh (r) * sin (i)));
  return v;
}

/* tanh(z) = (tanh(a) + itan(b)) / (1 - itanh(a)tan(b))  */
GFC_COMPLEX_8
ctanh (GFC_COMPLEX_8 a)
{
  GFC_REAL_8 rt;
  GFC_REAL_8 it;
  GFC_COMPLEX_8 n;
  GFC_COMPLEX_8 d;

  rt = tanh (REALPART (a));
  it = tan (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}

