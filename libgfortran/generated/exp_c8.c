/* Complex exponential functions
   Copyright 2002, 2004 Free Software Foundation, Inc.
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


/* z = a + ib  */
/* Absolute value.  */
GFC_REAL_8
cabs (GFC_COMPLEX_8 z)
{
  return hypot (REALPART (z), IMAGPART (z));
}

/* Complex argument.  The angle made with the +ve real axis.
   Range -pi-pi.  */
GFC_REAL_8
carg (GFC_COMPLEX_8 z)
{
  GFC_REAL_8 arg;

  return atan2 (IMAGPART (z), REALPART (z));
}

/* exp(z) = exp(a)*(cos(b) + isin(b))  */
GFC_COMPLEX_8
cexp (GFC_COMPLEX_8 z)
{
  GFC_REAL_8 a;
  GFC_REAL_8 b;
  GFC_COMPLEX_8 v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cos (b), sin (b));
  return exp (a) * v;
}

/* log(z) = log (cabs(z)) + i*carg(z)  */
GFC_COMPLEX_8
clog (GFC_COMPLEX_8 z)
{
  GFC_COMPLEX_8 v;

  COMPLEX_ASSIGN (v, log (cabs (z)), carg (z));
  return v;
}

/* log10(z) = log10 (cabs(z)) + i*carg(z)  */
GFC_COMPLEX_8
clog10 (GFC_COMPLEX_8 z)
{
  GFC_COMPLEX_8 v;

  COMPLEX_ASSIGN (v, log10 (cabs (z)), carg (z));
  return v;
}

/* pow(base, power) = cexp (power * clog (base))  */
GFC_COMPLEX_8
cpow (GFC_COMPLEX_8 base, GFC_COMPLEX_8 power)
{
  return cexp (power * clog (base));
}

/* sqrt(z).  Algorithm pulled from glibc.  */
GFC_COMPLEX_8
csqrt (GFC_COMPLEX_8 z)
{
  GFC_REAL_8 re;
  GFC_REAL_8 im;
  GFC_COMPLEX_8 v;

  re = REALPART (z);
  im = IMAGPART (z);
  if (im == 0.0)
    {
      if (re < 0.0)
        {
          COMPLEX_ASSIGN (v, 0.0, copysign (sqrt (-re), im));
        }
      else
        {
          COMPLEX_ASSIGN (v, fabs (sqrt (re)),
                          copysign (0.0, im));
        }
    }
  else if (re == 0.0)
    {
      GFC_REAL_8 r;

      r = sqrt (0.5 * fabs (im));

      COMPLEX_ASSIGN (v, copysign (r, im), r);
    }
  else
    {
      GFC_REAL_8 d, r, s;

      d = hypot (re, im);
      /* Use the identity   2  Re res  Im res = Im x
         to avoid cancellation error in  d +/- Re x.  */
      if (re > 0)
        {
          r = sqrt (0.5 * d + 0.5 * re);
          s = (0.5 * im) / r;
        }
      else
        {
          s = sqrt (0.5 * d - 0.5 * re);
          r = fabs ((0.5 * im) / s);
        }

      COMPLEX_ASSIGN (v, r, copysign (s, im));
    }
  return v;
}

