`/* Complex exponential functions
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
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
#include <math.h>
#include "libgfortran.h"'

include(`mtype.m4')dnl

/* z = a + ib  */
/* Absolute value.  */
real_type
cabs`'q (complex_type z)
{
  return hypot`'q (REALPART (z), IMAGPART (z));
}

/* Complex argument.  The angle made with the +ve real axis.
   Range -pi-pi.  */
real_type
carg`'q (complex_type z)
{
  real_type arg;

  return atan2`'q (IMAGPART (z), REALPART (z));
}

/* exp(z) = exp(a)*(cos(b) + isin(b))  */
complex_type
cexp`'q (complex_type z)
{
  real_type a;
  real_type b;
  complex_type v;

  a = REALPART (z);
  b = IMAGPART (z);
  COMPLEX_ASSIGN (v, cos`'q (b), sin`'q (b));
  return exp`'q (a) * v;
}

/* log(z) = log (cabs(z)) + i*carg(z)  */
complex_type
clog`'q (complex_type z)
{
  complex_type v;

  COMPLEX_ASSIGN (v, log`'q (cabs`'q (z)), carg`'q (z));
  return v;
}

/* log10(z) = log10 (cabs(z)) + i*carg(z)  */
complex_type
clog10`'q (complex_type z)
{
  complex_type v;

  COMPLEX_ASSIGN (v, log10`'q (cabs`'q (z)), carg`'q (z));
  return v;
}

/* pow(base, power) = cexp (power * clog (base))  */
complex_type
cpow`'q (complex_type base, complex_type power)
{
  return cexp`'q (power * clog`'q (base));
}

/* sqrt(z).  Algorithm pulled from glibc.  */
complex_type
csqrt`'q (complex_type z)
{
  real_type re;
  real_type im;
  complex_type v;

  re = REALPART (z);
  im = IMAGPART (z);
  if (im == 0.0)
    {
      if (re < 0.0)
        {
          COMPLEX_ASSIGN (v, 0.0, copysign`'q (sqrt`'q (-re), im));
        }
      else
        {
          COMPLEX_ASSIGN (v, fabs`'q (sqrt (re)),
                          copysign`'q (0.0, im));
        }
    }
  else if (re == 0.0)
    {
      real_type r;

      r = sqrt`'q (0.5 * fabs (im));

      COMPLEX_ASSIGN (v, copysign`'q (r, im), r);
    }
  else
    {
      real_type d, r, s;

      d = hypot`'q (re, im);
      /* Use the identity   2  Re res  Im res = Im x
         to avoid cancellation error in  d +/- Re x.  */
      if (re > 0)
        {
          r = sqrt`'q (0.5 * d + 0.5 * re);
          s = (0.5 * im) / r;
        }
      else
        {
          s = sqrt`'q (0.5 * d - 0.5 * re);
          r = fabs`'q ((0.5 * im) / s);
        }

      COMPLEX_ASSIGN (v, r, copysign`'q (s, im));
    }
  return v;
}

