/* Return arc hyperbolic sine for a complex float type, with the
   imaginary part of the result possibly adjusted for use in
   computing other functions.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#include "quadmath-imp.h"

/* Return the complex inverse hyperbolic sine of finite nonzero Z,
   with the imaginary part of the result subtracted from pi/2 if ADJ
   is nonzero.  */

__complex128
__quadmath_kernel_casinhq (__complex128 x, int adj)
{
  __complex128 res;
  __float128 rx, ix;
  __complex128 y;

  /* Avoid cancellation by reducing to the first quadrant.  */
  rx = fabsq (__real__ x);
  ix = fabsq (__imag__ x);

  if (rx >= 1 / FLT128_EPSILON || ix >= 1 / FLT128_EPSILON)
    {
      /* For large x in the first quadrant, x + csqrt (1 + x * x)
	 is sufficiently close to 2 * x to make no significant
	 difference to the result; avoid possible overflow from
	 the squaring and addition.  */
      __real__ y = rx;
      __imag__ y = ix;

      if (adj)
	{
	  __float128 t = __real__ y;
	  __real__ y = copysignq (__imag__ y, __imag__ x);
	  __imag__ y = t;
	}

      res = clogq (y);
      __real__ res += (__float128) M_LN2q;
    }
  else if (rx >= 0.5Q && ix < FLT128_EPSILON / 8)
    {
      __float128 s = hypotq (1, rx);

      __real__ res = logq (rx + s);
      if (adj)
	__imag__ res = atan2q (s, __imag__ x);
      else
	__imag__ res = atan2q (ix, s);
    }
  else if (rx < FLT128_EPSILON / 8 && ix >= 1.5Q)
    {
      __float128 s = sqrtq ((ix + 1) * (ix - 1));

      __real__ res = logq (ix + s);
      if (adj)
	__imag__ res = atan2q (rx, copysignq (s, __imag__ x));
      else
	__imag__ res = atan2q (s, rx);
    }
  else if (ix > 1 && ix < 1.5Q && rx < 0.5Q)
    {
      if (rx < FLT128_EPSILON * FLT128_EPSILON)
	{
	  __float128 ix2m1 = (ix + 1) * (ix - 1);
	  __float128 s = sqrtq (ix2m1);

	  __real__ res = log1pq (2 * (ix2m1 + ix * s)) / 2;
	  if (adj)
	    __imag__ res = atan2q (rx, copysignq (s, __imag__ x));
	  else
	    __imag__ res = atan2q (s, rx);
	}
      else
	{
	  __float128 ix2m1 = (ix + 1) * (ix - 1);
	  __float128 rx2 = rx * rx;
	  __float128 f = rx2 * (2 + rx2 + 2 * ix * ix);
	  __float128 d = sqrtq (ix2m1 * ix2m1 + f);
	  __float128 dp = d + ix2m1;
	  __float128 dm = f / dp;
	  __float128 r1 = sqrtq ((dm + rx2) / 2);
	  __float128 r2 = rx * ix / r1;

	  __real__ res = log1pq (rx2 + dp + 2 * (rx * r1 + ix * r2)) / 2;
	  if (adj)
	    __imag__ res = atan2q (rx + r1, copysignq (ix + r2, __imag__ x));
	  else
	    __imag__ res = atan2q (ix + r2, rx + r1);
	}
    }
  else if (ix == 1 && rx < 0.5Q)
    {
      if (rx < FLT128_EPSILON / 8)
	{
	  __real__ res = log1pq (2 * (rx + sqrtq (rx))) / 2;
	  if (adj)
	    __imag__ res = atan2q (sqrtq (rx), copysignq (1, __imag__ x));
	  else
	    __imag__ res = atan2q (1, sqrtq (rx));
	}
      else
	{
	  __float128 d = rx * sqrtq (4 + rx * rx);
	  __float128 s1 = sqrtq ((d + rx * rx) / 2);
	  __float128 s2 = sqrtq ((d - rx * rx) / 2);

	  __real__ res = log1pq (rx * rx + d + 2 * (rx * s1 + s2)) / 2;
	  if (adj)
	    __imag__ res = atan2q (rx + s1, copysignq (1 + s2, __imag__ x));
	  else
	    __imag__ res = atan2q (1 + s2, rx + s1);
	}
    }
  else if (ix < 1 && rx < 0.5Q)
    {
      if (ix >= FLT128_EPSILON)
	{
	  if (rx < FLT128_EPSILON * FLT128_EPSILON)
	    {
	      __float128 onemix2 = (1 + ix) * (1 - ix);
	      __float128 s = sqrtq (onemix2);

	      __real__ res = log1pq (2 * rx / s) / 2;
	      if (adj)
		__imag__ res = atan2q (s, __imag__ x);
	      else
		__imag__ res = atan2q (ix, s);
	    }
	  else
	    {
	      __float128 onemix2 = (1 + ix) * (1 - ix);
	      __float128 rx2 = rx * rx;
	      __float128 f = rx2 * (2 + rx2 + 2 * ix * ix);
	      __float128 d = sqrtq (onemix2 * onemix2 + f);
	      __float128 dp = d + onemix2;
	      __float128 dm = f / dp;
	      __float128 r1 = sqrtq ((dp + rx2) / 2);
	      __float128 r2 = rx * ix / r1;

	      __real__ res = log1pq (rx2 + dm + 2 * (rx * r1 + ix * r2)) / 2;
	      if (adj)
		__imag__ res = atan2q (rx + r1, copysignq (ix + r2,
							     __imag__ x));
	      else
		__imag__ res = atan2q (ix + r2, rx + r1);
	    }
	}
      else
	{
	  __float128 s = hypotq (1, rx);

	  __real__ res = log1pq (2 * rx * (rx + s)) / 2;
	  if (adj)
	    __imag__ res = atan2q (s, __imag__ x);
	  else
	    __imag__ res = atan2q (ix, s);
	}
      math_check_force_underflow_nonneg (__real__ res);
    }
  else
    {
      __real__ y = (rx - ix) * (rx + ix) + 1;
      __imag__ y = 2 * rx * ix;

      y = csqrtq (y);

      __real__ y += rx;
      __imag__ y += ix;

      if (adj)
	{
	  __float128 t = __real__ y;
	  __real__ y = copysignq (__imag__ y, __imag__ x);
	  __imag__ y = t;
	}

      res = clogq (y);
    }

  /* Give results the correct sign for the original argument.  */
  __real__ res = copysignq (__real__ res, __real__ x);
  __imag__ res = copysignq (__imag__ res, (adj ? 1 : __imag__ x));

  return res;
}
