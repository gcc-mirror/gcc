/* Implementation of gamma function according to ISO C.
   Copyright (C) 1997-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997 and
		  Jakub Jelinek <jj@ultra.linux.cz, 1999.

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
tgammaq (__float128 x)
{
  int sign;
  __float128 ret;
  ret = __quadmath_gammaq_r (x, &sign);
  return sign < 0 ? -ret : ret;
}

/* Coefficients B_2k / 2k(2k-1) of x^-(2k-1) inside exp in Stirling's
   approximation to gamma function.  */

static const __float128 gamma_coeff[] =
  {
    0x1.5555555555555555555555555555p-4Q,
    -0xb.60b60b60b60b60b60b60b60b60b8p-12Q,
    0x3.4034034034034034034034034034p-12Q,
    -0x2.7027027027027027027027027028p-12Q,
    0x3.72a3c5631fe46ae1d4e700dca8f2p-12Q,
    -0x7.daac36664f1f207daac36664f1f4p-12Q,
    0x1.a41a41a41a41a41a41a41a41a41ap-8Q,
    -0x7.90a1b2c3d4e5f708192a3b4c5d7p-8Q,
    0x2.dfd2c703c0cfff430edfd2c703cp-4Q,
    -0x1.6476701181f39edbdb9ce625987dp+0Q,
    0xd.672219167002d3a7a9c886459cp+0Q,
    -0x9.cd9292e6660d55b3f712eb9e07c8p+4Q,
    0x8.911a740da740da740da740da741p+8Q,
    -0x8.d0cc570e255bf59ff6eec24b49p+12Q,
  };

#define NCOEFF (sizeof (gamma_coeff) / sizeof (gamma_coeff[0]))

/* Return gamma (X), for positive X less than 1775, in the form R *
   2^(*EXP2_ADJ), where R is the return value and *EXP2_ADJ is set to
   avoid overflow or underflow in intermediate calculations.  */

static __float128
gammal_positive (__float128 x, int *exp2_adj)
{
  int local_signgam;
  if (x < 0.5Q)
    {
      *exp2_adj = 0;
      return expq (__quadmath_lgammaq_r (x + 1, &local_signgam)) / x;
    }
  else if (x <= 1.5Q)
    {
      *exp2_adj = 0;
      return expq (__quadmath_lgammaq_r (x, &local_signgam));
    }
  else if (x < 12.5Q)
    {
      /* Adjust into the range for using exp (lgamma).  */
      *exp2_adj = 0;
      __float128 n = ceilq (x - 1.5Q);
      __float128 x_adj = x - n;
      __float128 eps;
      __float128 prod = __quadmath_gamma_productq (x_adj, 0, n, &eps);
      return (expq (__quadmath_lgammaq_r (x_adj, &local_signgam))
	      * prod * (1 + eps));
    }
  else
    {
      __float128 eps = 0;
      __float128 x_eps = 0;
      __float128 x_adj = x;
      __float128 prod = 1;
      if (x < 24)
	{
	  /* Adjust into the range for applying Stirling's
	     approximation.  */
	  __float128 n = ceilq (24 - x);
	  x_adj = x + n;
	  x_eps = (x - (x_adj - n));
	  prod = __quadmath_gamma_productq (x_adj - n, x_eps, n, &eps);
	}
      /* The result is now gamma (X_ADJ + X_EPS) / (PROD * (1 + EPS)).
	 Compute gamma (X_ADJ + X_EPS) using Stirling's approximation,
	 starting by computing pow (X_ADJ, X_ADJ) with a power of 2
	 factored out.  */
      __float128 exp_adj = -eps;
      __float128 x_adj_int = roundq (x_adj);
      __float128 x_adj_frac = x_adj - x_adj_int;
      int x_adj_log2;
      __float128 x_adj_mant = frexpq (x_adj, &x_adj_log2);
      if (x_adj_mant < M_SQRT1_2q)
	{
	  x_adj_log2--;
	  x_adj_mant *= 2;
	}
      *exp2_adj = x_adj_log2 * (int) x_adj_int;
      __float128 ret = (powq (x_adj_mant, x_adj)
		       * exp2q (x_adj_log2 * x_adj_frac)
		       * expq (-x_adj)
		       * sqrtq (2 * M_PIq / x_adj)
		       / prod);
      exp_adj += x_eps * logq (x_adj);
      __float128 bsum = gamma_coeff[NCOEFF - 1];
      __float128 x_adj2 = x_adj * x_adj;
      for (size_t i = 1; i <= NCOEFF - 1; i++)
	bsum = bsum / x_adj2 + gamma_coeff[NCOEFF - 1 - i];
      exp_adj += bsum / x_adj;
      return ret + ret * expm1q (exp_adj);
    }
}

__float128
__quadmath_gammaq_r (__float128 x, int *signgamp)
{
  int64_t hx;
  uint64_t lx;
  __float128 ret;

  GET_FLT128_WORDS64 (hx, lx, x);

  if (((hx & 0x7fffffffffffffffLL) | lx) == 0)
    {
      /* Return value for x == 0 is Inf with divide by zero exception.  */
      *signgamp = 0;
      return 1.0 / x;
    }
  if (hx < 0 && (uint64_t) hx < 0xffff000000000000ULL && rintq (x) == x)
    {
      /* Return value for integer x < 0 is NaN with invalid exception.  */
      *signgamp = 0;
      return (x - x) / (x - x);
    }
  if (hx == 0xffff000000000000ULL && lx == 0)
    {
      /* x == -Inf.  According to ISO this is NaN.  */
      *signgamp = 0;
      return x - x;
    }
  if ((hx & 0x7fff000000000000ULL) == 0x7fff000000000000ULL)
    {
      /* Positive infinity (return positive infinity) or NaN (return
	 NaN).  */
      *signgamp = 0;
      return x + x;
    }

  if (x >= 1756)
    {
      /* Overflow.  */
      *signgamp = 0;
      return FLT128_MAX * FLT128_MAX;
    }
  else
    {
      SET_RESTORE_ROUNDF128 (FE_TONEAREST);
      if (x > 0)
	{
	  *signgamp = 0;
	  int exp2_adj;
	  ret = gammal_positive (x, &exp2_adj);
	  ret = scalbnq (ret, exp2_adj);
	}
      else if (x >= -FLT128_EPSILON / 4)
	{
	  *signgamp = 0;
	  ret = 1 / x;
	}
      else
	{
	  __float128 tx = truncq (x);
	  *signgamp = (tx == 2 * truncq (tx / 2)) ? -1 : 1;
	  if (x <= -1775)
	    /* Underflow.  */
	    ret = FLT128_MIN * FLT128_MIN;
	  else
	    {
	      __float128 frac = tx - x;
	      if (frac > 0.5Q)
		frac = 1 - frac;
	      __float128 sinpix = (frac <= 0.25Q
				  ? sinq (M_PIq * frac)
				  : cosq (M_PIq * (0.5Q - frac)));
	      int exp2_adj;
	      ret = M_PIq / (-x * sinpix
			     * gammal_positive (-x, &exp2_adj));
	      ret = scalbnq (ret, -exp2_adj);
	      math_check_force_underflow_nonneg (ret);
	    }
	}
    }
  if (isinfq (ret) && x != 0)
    {
      if (*signgamp < 0)
	return -(-copysignq (FLT128_MAX, ret) * FLT128_MAX);
      else
	return copysignq (FLT128_MAX, ret) * FLT128_MAX;
    }
  else if (ret == 0)
    {
      if (*signgamp < 0)
	return -(-copysignq (FLT128_MIN, ret) * FLT128_MIN);
      else
	return copysignq (FLT128_MIN, ret) * FLT128_MIN;
    }
  else
    return ret;
}
