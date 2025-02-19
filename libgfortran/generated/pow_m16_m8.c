/* Support routines for the intrinsic power (**) operator
   for UNSIGNED, using modulo arithmetic.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig.

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

#if defined (HAVE_GFC_UINTEGER_16) && defined (HAVE_GFC_UINTEGER_8)

GFC_UINTEGER_16 pow_m16_m8 (GFC_UINTEGER_16 x, GFC_UINTEGER_8 n);
export_proto(pow_m16_m8);

inline static GFC_UINTEGER_16
power_simple_m16_m8 (GFC_UINTEGER_16 x, GFC_UINTEGER_8 n)
{
  GFC_UINTEGER_16 pow = 1;
  for (;;)
    {
      if (n & 1)
	pow *= x;
      n >>= 1;
      if (n)
	x *= x;
      else
	break;
    }
  return pow; 
}

/* For odd x, Euler's theorem tells us that x**(2^(m-1)) = 1 mod 2^m.
   For even x, we use the fact that (2*x)^m = 0 mod 2^m.  */

GFC_UINTEGER_16
pow_m16_m8 (GFC_UINTEGER_16 x, GFC_UINTEGER_8 n)
{
  const GFC_UINTEGER_16 mask = (GFC_UINTEGER_16) (-1) / 2;
  if (n == 0)
    return 1;

  if  (x == 0)
    return 0;

  if (x & 1)
    return power_simple_m16_m8 (x, n & mask);

  if (n > sizeof (x) * 8)
    return 0;

  return power_simple_m16_m8 (x, n);
}

#endif
