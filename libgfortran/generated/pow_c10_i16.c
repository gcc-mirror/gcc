/* Support routines for the intrinsic power (**) operator.
   Copyright 2004, 2007 Free Software Foundation, Inc.
   Contributed by Paul Brook

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


/* Use Binary Method to calculate the powi. This is not an optimal but
   a simple and reasonable arithmetic. See section 4.6.3, "Evaluation of
   Powers" of Donald E. Knuth, "Seminumerical Algorithms", Vol. 2, "The Art
   of Computer Programming", 3rd Edition, 1998.  */

#if defined (HAVE_GFC_COMPLEX_10) && defined (HAVE_GFC_INTEGER_16)

GFC_COMPLEX_10 pow_c10_i16 (GFC_COMPLEX_10 a, GFC_INTEGER_16 b);
export_proto(pow_c10_i16);

GFC_COMPLEX_10
pow_c10_i16 (GFC_COMPLEX_10 a, GFC_INTEGER_16 b)
{
  GFC_COMPLEX_10 pow, x;
  GFC_INTEGER_16 n;
  GFC_UINTEGER_16 u;
  
  n = b;
  x = a;
  pow = 1;
  if (n != 0)
    {
      if (n < 0)
	{

	  u = -n;
	  x = pow / x;
	}
      else
	{
	   u = n;
	}
      for (;;)
	{
	  if (u & 1)
	    pow *= x;
	  u >>= 1;
	  if (u)
	    x *= x;
	  else
	    break;
	}
    }
  return pow;
}

#endif
