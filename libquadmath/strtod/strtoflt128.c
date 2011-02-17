/* Copyright (C) 1999, 2004 Free Software Foundation, Inc.
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

/* The actual implementation for all floating point sizes is in strtod.c.
   These macros tell it to produce the `__float128' version, `strtold'.  */

#define FLOAT		__float128
#define FLT		FLT128
#ifdef USE_WIDE_CHAR
# define STRTOF		wcstoflt128
# define __STRTOF	__wcstoflt128
#else
# define STRTOF		strtoflt128
# define __STRTOF	__strtoflt128
#endif
#define MPN2FLOAT	mpn_construct_float128
#define FLOAT_HUGE_VAL	HUGE_VALQ
#define SET_MANTISSA(flt, mant) \
  do { ieee854_float128 u;						      \
       u.value = (flt);							      \
       u.ieee.mant_high = 0x800000000000ULL;				      \
       u.ieee.mant_low = mant;						      \
       (flt) = u.value;							      \
  } while (0)

static inline __attribute__((__always_inline__))
__float128 ____strtoflt128_internal (const char *, char **, int);

#include "strtod_l.c"

__float128
strtoflt128 (const char *nptr, char **endptr)
{
  return ____STRTOF_INTERNAL (nptr, endptr, 0);
}
