`/* Complex hyperbolic functions
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
#include <math.h>
#include "libgfortran.h"'

include(`mtype.m4')dnl

/* Complex number z = a + ib.  */

/* sinh(z) = sinh(a)cos(b) + icosh(a)sin(b)  */
complex_type
csinh`'q (complex_type a)
{
  real_type r;
  real_type i;
  complex_type v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, sinh`'q (r) * cos`'q (i), cosh`'q (r) * sin`'q (i));
  return v;
}

/* cosh(z) = cosh(a)cos(b) - isinh(a)sin(b)  */
complex_type
ccosh`'q (complex_type a)
{
  real_type r;
  real_type i;
  complex_type v;

  r = REALPART (a);
  i = IMAGPART (a);
  COMPLEX_ASSIGN (v, cosh`'q (r) * cos`'q (i), - (sinh`'q (r) * sin`'q (i)));
  return v;
}

/* tanh(z) = (tanh(a) + itan(b)) / (1 - itanh(a)tan(b))  */
complex_type
ctanh`'q (complex_type a)
{
  real_type rt;
  real_type it;
  complex_type n;
  complex_type d;

  rt = tanh`'q (REALPART (a));
  it = tan`'q (IMAGPART (a));
  COMPLEX_ASSIGN (n, rt, it);
  COMPLEX_ASSIGN (d, 1, - (rt * it));

  return n / d;
}

