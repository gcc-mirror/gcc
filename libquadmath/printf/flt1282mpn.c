/* Copyright (C) 1995,1996,1997,1998,1999,2002,2003
	Free Software Foundation, Inc.
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
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <float.h>
#include <math.h>
#include <stdlib.h>
#include "gmp-impl.h"

/* Convert a `__float128' in IEEE854 quad-precision format to a
   multi-precision integer representing the significand scaled up by its
   number of bits (113 for long double) and an integral power of two
   (MPN frexpl). */

mp_size_t
mpn_extract_flt128 (mp_ptr res_ptr, mp_size_t size,
		      int *expt, int *is_neg,
		      __float128 value)
{
  ieee854_float128 u;
  u.value = value;

  *is_neg = u.ieee.negative;
  *expt = (int) u.ieee.exponent - IEEE854_FLOAT128_BIAS;

#if BITS_PER_MP_LIMB == 32
  res_ptr[0] = u.ieee.mant_low; /* Low-order 32 bits of fraction.  */
  res_ptr[1] = (u.ieee.mant_low >> 32);
  res_ptr[2] = u.ieee.mant_high;
  res_ptr[3] = (u.ieee.mant_high >> 32); /* High-order 32 bits.  */
  #define N 4
#elif BITS_PER_MP_LIMB == 64
  res_ptr[0] = u.ieee.mant_low;
  res_ptr[1] = u.ieee.mant_high;
  #define N 2
#else
  #error "mp_limb size " BITS_PER_MP_LIMB "not accounted for"
#endif
/* The format does not fill the last limb.  There are some zeros.  */
#define NUM_LEADING_ZEROS (BITS_PER_MP_LIMB \
			   - (FLT128_MANT_DIG - ((N - 1) * BITS_PER_MP_LIMB)))

  if (u.ieee.exponent == 0)
    {
      /* A biased exponent of zero is a special case.
	 Either it is a zero or it is a denormal number.  */
      if (res_ptr[0] == 0 && res_ptr[1] == 0
	  && res_ptr[N - 2] == 0 && res_ptr[N - 1] == 0) /* Assumes N<=4.  */
	/* It's zero.  */
	*expt = 0;
      else
	{
	  /* It is a denormal number, meaning it has no implicit leading
  	     one bit, and its exponent is in fact the format minimum.  */
	  int cnt;

#if N == 2
	  if (res_ptr[N - 1] != 0)
	    {
	      count_leading_zeros (cnt, res_ptr[N - 1]);
	      cnt -= NUM_LEADING_ZEROS;
	      res_ptr[N - 1] = res_ptr[N - 1] << cnt
			       | (res_ptr[0] >> (BITS_PER_MP_LIMB - cnt));
	      res_ptr[0] <<= cnt;
	      *expt = FLT128_MIN_EXP - 1 - cnt;
	    }
	  else
	    {
	      count_leading_zeros (cnt, res_ptr[0]);
	      if (cnt >= NUM_LEADING_ZEROS)
		{
		  res_ptr[N - 1] = res_ptr[0] << (cnt - NUM_LEADING_ZEROS);
		  res_ptr[0] = 0;
		}
	      else
		{
		  res_ptr[N - 1] = res_ptr[0] >> (NUM_LEADING_ZEROS - cnt);
		  res_ptr[0] <<= BITS_PER_MP_LIMB - (NUM_LEADING_ZEROS - cnt);
		}
	      *expt = FLT128_MIN_EXP - 1
		- (BITS_PER_MP_LIMB - NUM_LEADING_ZEROS) - cnt;
	    }
#else
	  int j, k, l;

	  for (j = N - 1; j > 0; j--)
	    if (res_ptr[j] != 0)
	      break;

	  count_leading_zeros (cnt, res_ptr[j]);
	  cnt -= NUM_LEADING_ZEROS;
	  l = N - 1 - j;
	  if (cnt < 0)
	    {
	      cnt += BITS_PER_MP_LIMB;
	      l--;
	    }
	  if (!cnt)
	    for (k = N - 1; k >= l; k--)
	      res_ptr[k] = res_ptr[k-l];
	  else
	    {
	      for (k = N - 1; k > l; k--)
		res_ptr[k] = res_ptr[k-l] << cnt
			     | res_ptr[k-l-1] >> (BITS_PER_MP_LIMB - cnt);
	      res_ptr[k--] = res_ptr[0] << cnt;
	    }

	  for (; k >= 0; k--)
	    res_ptr[k] = 0;
	  *expt = FLT128_MIN_EXP - 1 - l * BITS_PER_MP_LIMB - cnt;
#endif
	}
    }
  else
    /* Add the implicit leading one bit for a normalized number.  */
    res_ptr[N - 1] |= (mp_limb_t) 1 << (FLT128_MANT_DIG - 1
					- ((N - 1) * BITS_PER_MP_LIMB));

  return N;
}
