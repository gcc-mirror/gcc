/* Compute x * y + z as ternary operation.
   Copyright (C) 2010 Free Software Foundation, Inc.
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
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

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
      /* If x or y or z is Inf/NaN, or if fma will certainly overflow,
	 or if x * y is less than half of FLT128_DENORM_MIN,
	 compute as x * y + z.  */
      if (u.ieee.exponent == 0x7fff
	  || v.ieee.exponent == 0x7fff
	  || w.ieee.exponent == 0x7fff
	  || u.ieee.exponent + v.ieee.exponent
	     > 0x7fff + IEEE854_FLOAT128_BIAS
	  || u.ieee.exponent + v.ieee.exponent
	     < IEEE854_FLOAT128_BIAS - FLT128_MANT_DIG - 2)
	return x * y + z;
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
	     very small, it doesn't matter if we don't adjust it.  */
	  if (u.ieee.exponent > v.ieee.exponent)
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
	    u.ieee.exponent += 2 * FLT128_MANT_DIG;
	  else
	    v.ieee.exponent += 2 * FLT128_MANT_DIG;
	  if (w.ieee.exponent <= 4 * FLT128_MANT_DIG + 4)
	    {
	      if (w.ieee.exponent)
		w.ieee.exponent += 2 * FLT128_MANT_DIG;
	      else
		w.value *= 0x1p226Q;
	      adjust = -1;
	    }
	  /* Otherwise x * y should just affect inexact
	     and nothing else.  */
	}
      x = u.value;
      y = v.value;
      z = w.value;
    }
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

#ifdef USE_FENV_H
  fenv_t env;
  feholdexcept (&env);
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
      asm volatile ("" : : "m" (v));
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
	return v.value * 0x1p-226Q;
      /* If result rounded to zero is not subnormal, no double
	 rounding will occur.  */
      if (v.ieee.exponent > 226)
	return (a1 + u.value) * 0x1p-226Q;
      /* If v.value * 0x1p-226Q with round to zero is a subnormal above
	 or equal to FLT128_MIN / 2, then v.value * 0x1p-226Q shifts mantissa
	 down just by 1 bit, which means v.ieee.mant_low |= j would
	 change the round bit, not sticky or guard bit.
	 v.value * 0x1p-226Q never normalizes by shifting up,
	 so round bit plus sticky bit should be already enough
	 for proper rounding.  */
      if (v.ieee.exponent == 226)
	{
	  /* v.ieee.mant_low & 2 is LSB bit of the result before rounding,
	     v.ieee.mant_low & 1 is the round bit and j is our sticky
	     bit.  In round-to-nearest 001 rounds down like 00,
	     011 rounds up, even though 01 rounds down (thus we need
	     to adjust), 101 rounds down like 10 and 111 rounds up
	     like 11.  */
	  if ((v.ieee.mant_low & 3) == 1)
	    {
	      v.value *= 0x1p-226Q;
	      if (v.ieee.negative)
		return v.value - 0x1p-16494Q /* __FLT128_DENORM_MIN__ */;
	      else
		return v.value + 0x1p-16494Q /* __FLT128_DENORM_MIN__ */;
	    }
	  else
	    return v.value * 0x1p-226Q;
	}
      v.ieee.mant_low |= j;
      return v.value * 0x1p-226Q;
    }
}
