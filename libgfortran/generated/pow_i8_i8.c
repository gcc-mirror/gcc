/* Support routines for the intrinsic power (**) operator.
   Copyright 2004 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Ligbfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"

/* Uuse Binary Method to calculate the powi. This is not an optimal but
   a simple and reasonable arithmetic. See section 4.6.3, "Evaluation of
   Powers" of Donald E. Knuth, "Seminumerical Algorithms", Vol. 2, "The Art
   of Computer Programming", 3rd Edition, 1998.  */

GFC_INTEGER_8
prefix(pow_i8_i8) (GFC_INTEGER_8 a, GFC_INTEGER_8 b)
{
  GFC_INTEGER_8 pow, x;
  GFC_INTEGER_8 n, u;
  
  n = b;
  x = a;
  pow = 1;
  if (n != 0)
    {
      if (n < 0)
	{
	  if (x == 1)
	    return 1;
	  if (x == -1)
	    return (n & 1) ? -1 : 1;
	  return (x == 0) ? 1 / x : 0;
	}
      u = n;
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
