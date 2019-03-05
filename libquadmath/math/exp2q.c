/* Compute 2^x.
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

__float128
exp2q (__float128 x)
{
  if (__glibc_likely (__builtin_isless (x, (__float128) FLT128_MAX_EXP)))
    {
      if (__builtin_expect (__builtin_isgreaterequal (x, (__float128) (FLT128_MIN_EXP - FLT128_MANT_DIG
							- 1)), 1))
	{
	  int intx = (int) x;
	  __float128 fractx = x - intx;
	  __float128 result;
	  if (fabsq (fractx) < FLT128_EPSILON / 4)
	    result = scalbnq (1 + fractx, intx);
	  else
	    result = scalbnq (expq (M_LN2q * fractx), intx);
	  math_check_force_underflow_nonneg (result);
	  return result;
	}
      else
	{
	  /* Underflow or exact zero.  */
	  if (isinfq (x))
	    return 0;
	  else
	    return FLT128_MIN * FLT128_MIN;
	}
    }
  else
    /* Infinity, NaN or overflow.  */
    return FLT128_MAX * x;
}
