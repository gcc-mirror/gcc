/* Support routines for the intrinsic power (**) operator.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Paul Brook

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


/* Use Binary Method to calculate the powi. This is not an optimal but
   a simple and reasonable arithmetic. See section 4.6.3, "Evaluation of
   Powers" of Donald E. Knuth, "Seminumerical Algorithms", Vol. 2, "The Art
   of Computer Programming", 3rd Edition, 1998.  */

#if defined (HAVE_GFC_REAL_8) && defined (HAVE_GFC_INTEGER_8)

GFC_REAL_8 pow_r8_i8 (GFC_REAL_8 a, GFC_INTEGER_8 b);
export_proto(pow_r8_i8);

GFC_REAL_8
pow_r8_i8 (GFC_REAL_8 a, GFC_INTEGER_8 b)
{
  GFC_REAL_8 pow, x;
  GFC_INTEGER_8 n;
  GFC_UINTEGER_8 u;
  
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
