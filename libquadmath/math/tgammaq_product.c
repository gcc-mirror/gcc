/* Compute a product of X, X+1, ..., with an error estimate.
   Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

/* Compute the product of X + X_EPS, X + X_EPS + 1, ..., X + X_EPS + N
   - 1, in the form R * (1 + *EPS) where the return value R is an
   approximation to the product and *EPS is set to indicate the
   approximate error in the return value.  X is such that all the
   values X + 1, ..., X + N - 1 are exactly representable, and X_EPS /
   X is small enough that factors quadratic in it can be
   neglected.  */

__float128
__quadmath_gamma_productq (__float128 x, __float128 x_eps, int n, __float128 *eps)
{
  SET_RESTORE_ROUNDF128 (FE_TONEAREST);
  __float128 ret = x;
  *eps = x_eps / x;
  for (int i = 1; i < n; i++)
    {
      *eps += x_eps / (x + i);
      __float128 lo;
      mul_splitq (&ret, &lo, ret, x + i);
      *eps += lo / ret;
    }
  return ret;
}
