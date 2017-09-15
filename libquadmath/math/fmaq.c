/* Compute x * y + z as ternary operation.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jakub Jelinek <jakub@redhat.com>, 2010.

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
#include <math.h>
#include <float.h>
#ifdef HAVE_FENV_H
# include <fenv.h>
# if defined HAVE_FEHOLDEXCEPT && defined HAVE_FESETROUND \
     && defined HAVE_FEUPDATEENV && defined HAVE_FETESTEXCEPT \
     && defined FE_TOWARDZERO && defined FE_INEXACT
#  define USE_FENV_H
# endif
#endif

/* This implementation uses rounding to odd to avoid problems with
   double rounding.  See a paper by Boldo and Melquiond:
   http://www.lri.fr/~melquion/doc/08-tc.pdf  */

__float128
fmaq (__float128 x, __float128 y, __float128 z)
{
  ieee854_float128 u, v, w;
  int adjust = 0;
  u.value = x;
  v.value = y;
  w.value = z;
  if (__builtin_expect (u.ieee.exponent + v.ieee.exponent
			>= 0x7fff + IEEE854_FLOAT128_BIAS
			   - FLT128_MANT_DIG, 0)
      || __builtin_expect (u.ieee.exponent >= 0x7fff - FLT128_MANT_DIG, 0)
      || __builtin_expect (v.ieee.exponent >= 0x7fff - FLT128_MANT_DIG, 0)
      || __builtin_expect (w.ieee.exponent >= 0x7fff - FLT128_MANT_DIG, 0)
      || __builtin_expect (u.ieee.exponent + v.ieee.exponent
			   <= IEEE854_FLOAT128_BIAS + FLT128_MANT_DIG, 0))
    {
      /* If z is Inf, but x and y are finite, the result should be
	 z rather than NaN.  */
      if (w.ieee.exponent == 0x7fff
	  && u.ieee.exponent != 0x7fff
          && v.ieee.exponent != 0x7fff)
	return (z + x) + y;
      /* If z is zero and x are y are nonzero, compute the result
	 as x * y to avoid the wrong sign of a zero result if x * y
	 underflows to 0.  */
      if (z == 0 && x != 0 && y != 0)
	return x * y;
      /* If x or y or z is Inf/NaN, or if x * y is zero, compute as
	 x * y + z.  */
      if (u.ieee.exponent == 0x7fff
	  || v.ieee.exponent == 0x7fff
	  || w.ieee.exponent == 0x7fff
	  || x == 0
	  || y == 0)
	return x * y + z;
      /* If fma will certainly overflow, compute as x * y.  */
      if (u.ieee.exponent + v.ieee.exponent
	  > 0x7fff + IEEE854_FLOAT128_BIAS)
	return x * y;
      /* If x * y is less than 1/4 of FLT128_DENORM_MIN, neither the
	 result nor whether there is underflow depends on its exact
	 value, only on its sign.  */
      if (u.ieee.exponent + v.ieee.exponent
	  < IEEE854_FLOAT128_BIAS - FLT128_MANT_DIG - 2)
	{
	  int neg = u.ieee.negative ^ v.ieee.negative;
	  __float128 tiny = neg ? -0x1p-16494Q : 0x1p-16494Q;
	  if (w.ieee.exponent >= 3)
	    return tiny + z;
	  /* Scaling up, adding TINY and scaling down produces the
	     correct result, because in round-to-nearest mode adding
	     TINY has no effect and in other modes double rounding is
	     harmless.  But it may not produce required underflow
	     exceptions.  */
	  v.value = z * 0x1p114Q + tiny;
	  if (TININESS_AFTER_ROUNDING
	      ? v.ieee.exponent < 115
	      : (w.ieee.exponent == 0
		 || (w.ieee.exponent == 1
		     && w.ieee.negative != neg
		     && w.ieee.mant_low == 0
		     && w.ieee.mant_high == 0)))
	    {
	      __float128 force_underflow = x * y;
	      math_force_eval (force_underflow);
	    }
	  return v.value * 0x1p-114Q;
	}
      if (u.ieee.exponent + v.ieee.exponent
	  >= 0x7fff + IEEE854_FLOAT128_BIAS - FLT128_MANT_DIG)
	{
	  /* Compute 1p-113 times smaller result and multiply
	     at the end.  */
	  if (u.ieee.exponent > v.ieee.exponent)
	    u.ieee.exponent -= FLT128_MANT_DIG;
	  else
	    v.ieee.exponent -= FLT128_MANT_DIG;
	  /* If x + y exponent is very large and z exponent is very small,
	     it doesn't matter if we don't adjust it.  */
	  if (w.ieee.exponent > FLT128_MANT_DIG)
	    w.ieee.exponent -= FLT128_MANT_DIG;
	  adjust = 1;
	}
      else if (w.ieee.exponent >= 0x7fff - FLT128_MANT_DIG)
	{
	  /* Similarly.
	     If z exponent is very large and x and y exponents are
	     very small, adjust them up to avoid spurious underflows,
	     rather than down.  */
	  if (u.ieee.exponent + v.ieee.exponent
	      <= IEEE854_FLOAT128_BIAS + FLT128_MANT_DIG)
	    {
	      if (u.ieee.exponent > v.ieee.exponent)
		u.ieee.exponent += 2 * FLT128_MANT_DIG + 2;
	      else
		v.ieee.exponent += 2 * FLT128_MANT_DIG + 2;
	    }
	  else if (u.ieee.exponent > v.ieee.exponent)
	    {
	      if (u.ieee.exponent > FLT128_MANT_DIG)
		u.ieee.exponent -= FLT128_MANT_DIG;
	    }
	  else if (v.ieee.exponent > FLT128_MANT_DIG)
	    v.ieee.exponent -= FLT128_MANT_DIG;
	  w.ieee.exponent -= FLT128_MANT_DIG;
	  adjust = 1;
	}
      else if (u.ieee.exponent >= 0x7fff - FLT128_MANT_DIG)
	{
	  u.ieee.exponent -= FLT128_MANT_DIG;
	  if (v.ieee.exponent)
	    v.ieee.exponent += FLT128_MANT_DIG;
	  else
	    v.value *= 0x1p113Q;
	}
      else if (v.ieee.exponent >= 0x7fff - FLT128_MANT_DIG)
	{
	  v.ieee.exponent -= FLT128_MANT_DIG;
	  if (u.ieee.exponent)
	    u.ieee.exponent += FLT128_MANT_DIG;
	  else
	    u.value *= 0x1p113Q;
	}
      else /* if (u.ieee.exponent + v.ieee.exponent
		  <= IEEE854_FLOAT128_BIAS + FLT128_MANT_DIG) */
	{
	  if (u.ieee.exponent > v.ieee.exponent)
	    u.ieee.exponent += 2 * FLT128_MANT_DIG + 2;
	  else
	    v.ieee.exponent += 2 * FLT128_MANT_DIG + 2;
	  if (w.ieee.exponent <= 4 * FLT128_MANT_DIG + 6)
	    {
	      if (w.ieee.exponent)
		w.ieee.exponent += 2 * FLT128_MANT_DIG + 2;
	      else
		w.value *= 0x1p228Q;
	      adjust = -1;
	    }
	  /* Otherwise x * y should just affect inexact
	     and nothing else.  */
	}
      x = u.value;
      y = v.value;
      z = w.value;
    }

  /* Ensure correct sign of exact 0 + 0.  */
  if (__builtin_expect ((x == 0 || y == 0) && z == 0, 0))
    {
      x = math_opt_barrier (x);
      return x * y + z;
    }

#ifdef USE_FENV_H
  fenv_t env;
  feholdexcept (&env);
  fesetround (FE_TONEAREST);
#endif

  /* Multiplication m1 + m2 = x * y using Dekker's algorithm.  */
#define C ((1LL << (FLT128_MANT_DIG + 1) / 2) + 1)
  __float128 x1 = x * C;
  __float128 y1 = y * C;
  __float128 m1 = x * y;
  x1 = (x - x1) + x1;
  y1 = (y - y1) + y1;
  __float128 x2 = x - x1;
  __float128 y2 = y - y1;
  __float128 m2 = (((x1 * y1 - m1) + x1 * y2) + x2 * y1) + x2 * y2;

  /* Addition a1 + a2 = z + m1 using Knuth's algorithm.  */
  __float128 a1 = z + m1;
  __float128 t1 = a1 - z;
  __float128 t2 = a1 - t1;
  t1 = m1 - t1;
  t2 = z - t2;
  __float128 a2 = t1 + t2;
  /* Ensure the arithmetic is not scheduled after feclearexcept call.  */
  math_force_eval (m2);
  math_force_eval (a2);
#ifdef USE_FENV_H
  feclearexcept (FE_INEXACT);
#endif

  /* If the result is an exact zero, ensure it has the correct sign.  */
  if (a1 == 0 && m2 == 0)
    {
#ifdef USE_FENV_H
      feupdateenv (&env);
#endif
      /* Ensure that round-to-nearest value of z + m1 is not reused.  */
      z = math_opt_barrier (z);
      return z + m1;
    }

#ifdef USE_FENV_H
  fesetround (FE_TOWARDZERO);
#endif
  /* Perform m2 + a2 addition with round to odd.  */
  u.value = a2 + m2;

  if (__builtin_expect (adjust == 0, 1))
    {
#ifdef USE_FENV_H
      if ((u.ieee.mant_low & 1) == 0 && u.ieee.exponent != 0x7fff)
	u.ieee.mant_low |= fetestexcept (FE_INEXACT) != 0;
      feupdateenv (&env);
#endif
      /* Result is a1 + u.value.  */
      return a1 + u.value;
    }
  else if (__builtin_expect (adjust > 0, 1))
    {
#ifdef USE_FENV_H
      if ((u.ieee.mant_low & 1) == 0 && u.ieee.exponent != 0x7fff)
	u.ieee.mant_low |= fetestexcept (FE_INEXACT) != 0;
      feupdateenv (&env);
#endif
      /* Result is a1 + u.value, scaled up.  */
      return (a1 + u.value) * 0x1p113Q;
    }
  else
    {
#ifdef USE_FENV_H
      if ((u.ieee.mant_low & 1) == 0)
	u.ieee.mant_low |= fetestexcept (FE_INEXACT) != 0;
#endif
      v.value = a1 + u.value;
      /* Ensure the addition is not scheduled after fetestexcept call.  */
      asm volatile ("" : : "m" (v.value));
#ifdef USE_FENV_H
      int j = fetestexcept (FE_INEXACT) != 0;
      feupdateenv (&env);
#else
      int j = 0;
#endif
      /* Ensure the following computations are performed in default rounding
	 mode instead of just reusing the round to zero computation.  */
      asm volatile ("" : "=m" (u) : "m" (u));
      /* If a1 + u.value is exact, the only rounding happens during
	 scaling down.  */
      if (j == 0)
	return v.value * 0x1p-228Q;
      /* If result rounded to zero is not subnormal, no double
	 rounding will occur.  */
      if (v.ieee.exponent > 228)
	return (a1 + u.value) * 0x1p-228Q;
      /* If v.value * 0x1p-228Q with round to zero is a subnormal above
	 or equal to FLT128_MIN / 2, then v.value * 0x1p-228Q shifts mantissa
	 down just by 1 bit, which means v.ieee.mant_low |= j would
	 change the round bit, not sticky or guard bit.
	 v.value * 0x1p-228Q never normalizes by shifting up,
	 so round bit plus sticky bit should be already enough
	 for proper rounding.  */
      if (v.ieee.exponent == 228)
	{
	  /* If the exponent would be in the normal range when
	     rounding to normal precision with unbounded exponent
	     range, the exact result is known and spurious underflows
	     must be avoided on systems detecting tininess after
	     rounding.  */
	  if (TININESS_AFTER_ROUNDING)
	    {
	      w.value = a1 + u.value;
	      if (w.ieee.exponent == 229)
		return w.value * 0x1p-228Q;
	    }
	  /* v.ieee.mant_low & 2 is LSB bit of the result before rounding,
	     v.ieee.mant_low & 1 is the round bit and j is our sticky
	     bit. */
	  w.value = 0.0Q;
	  w.ieee.mant_low = ((v.ieee.mant_low & 3) << 1) | j;
	  w.ieee.negative = v.ieee.negative;
	  v.ieee.mant_low &= ~3U;
	  v.value *= 0x1p-228Q;
	  w.value *= 0x1p-2Q;
	  return v.value + w.value;
	}
      v.ieee.mant_low |= j;
      return v.value * 0x1p-228Q;
    }
}
