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

#include <config.h>
#include <float.h>
#include <math.h>
#include "../printf/gmp-impl.h"

/* Convert a multi-precision integer of the needed number of bits (113 for
   long double) and an integral power of two to a `long double' in IEEE854
   quad-precision format.  */

__float128
mpn_construct_float128 (mp_srcptr frac_ptr, int expt, int sign)
{
  ieee854_float128 u;

  u.ieee.negative = sign;
  u.ieee.exponent = expt + IEEE854_FLOAT128_BIAS;
#if BITS_PER_MP_LIMB == 32
  u.ieee.mant_low = (((uint64_t) frac_ptr[1]) << 32)
		    | (frac_ptr[0] & 0xffffffff);
  u.ieee.mant_high = (((uint64_t) frac_ptr[3]
		       & (((mp_limb_t) 1 << (FLT128_MANT_DIG - 96)) - 1))
		      << 32) | (frac_ptr[2] & 0xffffffff);
#elif BITS_PER_MP_LIMB == 64
  u.ieee.mant_low = frac_ptr[0];
  u.ieee.mant_high = frac_ptr[1]
		     & (((mp_limb_t) 1 << (FLT128_MANT_DIG - 64)) - 1);
#else
  #error "mp_limb size " BITS_PER_MP_LIMB "not accounted for"
#endif

  return u.value;
}
