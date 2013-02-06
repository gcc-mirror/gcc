/* Include file for internal GNU MP types and definitions.

Copyright (C) 1991, 1993, 1994, 1995, 1996, 2011 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include <stdlib.h>
#include "quadmath-imp.h"

#undef alloca
#define alloca __builtin_alloca

#define ABS(x) (x >= 0 ? x : -x)
#ifndef MIN
#define MIN(l,o) ((l) < (o) ? (l) : (o))
#endif
#ifndef MAX
#define MAX(h,i) ((h) > (i) ? (h) : (i))
#endif

#define BITS_PER_MP_LIMB (__SIZEOF_LONG__ * __CHAR_BIT__)
#define BYTES_PER_MP_LIMB (BITS_PER_MP_LIMB / __CHAR_BIT__)
typedef unsigned long int	mp_limb_t;
typedef long int		mp_limb_signed_t;

typedef mp_limb_t *             mp_ptr;
typedef const mp_limb_t *	mp_srcptr;
typedef long int                mp_size_t;
typedef long int                mp_exp_t;

/* Define stuff for longlong.h.  */
typedef unsigned int UQItype	__attribute__ ((mode (QI)));
typedef 	 int SItype	__attribute__ ((mode (SI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int UDItype	__attribute__ ((mode (DI)));

typedef mp_limb_t UWtype;
typedef unsigned int UHWtype;
#define W_TYPE_SIZE BITS_PER_MP_LIMB

#ifdef HAVE_HIDDEN_VISIBILITY
#define attribute_hidden __attribute__((__visibility__ ("hidden")))
#else
#define attribute_hidden
#endif

#include "../../libgcc/longlong.h"

/* Copy NLIMBS *limbs* from SRC to DST.  */
#define MPN_COPY_INCR(DST, SRC, NLIMBS) \
  do {									\
    mp_size_t __i;							\
    for (__i = 0; __i < (NLIMBS); __i++)				\
      (DST)[__i] = (SRC)[__i];						\
  } while (0)
#define MPN_COPY_DECR(DST, SRC, NLIMBS) \
  do {									\
    mp_size_t __i;							\
    for (__i = (NLIMBS) - 1; __i >= 0; __i--)				\
      (DST)[__i] = (SRC)[__i];						\
  } while (0)
#define MPN_COPY MPN_COPY_INCR

/* Zero NLIMBS *limbs* AT DST.  */
#define MPN_ZERO(DST, NLIMBS) \
  do {									\
    mp_size_t __i;							\
    for (__i = 0; __i < (NLIMBS); __i++)				\
      (DST)[__i] = 0;							\
  } while (0)

#define MPN_MUL_N_RECURSE(prodp, up, vp, size, tspace) \
  do {									\
    if ((size) < KARATSUBA_THRESHOLD)					\
      impn_mul_n_basecase (prodp, up, vp, size);			\
    else								\
      impn_mul_n (prodp, up, vp, size, tspace);			\
  } while (0);

#define __MPN(x) __quadmath_mpn_##x

/* Internal mpn calls */
#define impn_mul_n_basecase	__MPN(impn_mul_n_basecase)
#define impn_mul_n		__MPN(impn_mul_n)

/* Prototypes for internal mpn calls.  */
void impn_mul_n_basecase (mp_ptr prodp, mp_srcptr up, mp_srcptr vp,
			  mp_size_t size) attribute_hidden;
void impn_mul_n (mp_ptr prodp, mp_srcptr up, mp_srcptr vp, mp_size_t size,
		 mp_ptr tspace) attribute_hidden;

#define mpn_add_n		__MPN(add_n)
#define mpn_addmul_1		__MPN(addmul_1)
#define mpn_cmp			__MPN(cmp)
#define mpn_divrem		__MPN(divrem)
#define mpn_lshift		__MPN(lshift)
#define mpn_mul			__MPN(mul)
#define mpn_mul_1		__MPN(mul_1)
#define mpn_rshift		__MPN(rshift)
#define mpn_sub_n		__MPN(sub_n)
#define mpn_submul_1		__MPN(submul_1)

mp_limb_t mpn_add_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t)
     attribute_hidden;
mp_limb_t mpn_addmul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t)
     attribute_hidden;
int mpn_cmp (mp_srcptr, mp_srcptr, mp_size_t) attribute_hidden;
mp_limb_t mpn_divrem (mp_ptr, mp_size_t, mp_ptr, mp_size_t, mp_srcptr,
		      mp_size_t) attribute_hidden;
mp_limb_t mpn_lshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int)
     attribute_hidden;
mp_limb_t mpn_mul (mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t)
     attribute_hidden;
mp_limb_t mpn_mul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t)
     attribute_hidden;
mp_limb_t mpn_rshift (mp_ptr, mp_srcptr, mp_size_t, unsigned int)
     attribute_hidden;
mp_limb_t mpn_sub_n (mp_ptr, mp_srcptr, mp_srcptr, mp_size_t)
     attribute_hidden;
mp_limb_t mpn_submul_1 (mp_ptr, mp_srcptr, mp_size_t, mp_limb_t)
     attribute_hidden;

#define mpn_extract_flt128 __MPN(extract_flt128)
mp_size_t mpn_extract_flt128 (mp_ptr res_ptr, mp_size_t size, int *expt,
			      int *is_neg, __float128 value) attribute_hidden;

#define mpn_construct_float128 __MPN(construct_float128)
__float128 mpn_construct_float128 (mp_srcptr frac_ptr, int expt, int sign)
     attribute_hidden;

#define mpn_divmod(qp,np,nsize,dp,dsize) mpn_divrem (qp,0,np,nsize,dp,dsize)

static inline mp_limb_t
mpn_add_1 (register mp_ptr res_ptr,
	   register mp_srcptr s1_ptr,
	   register mp_size_t s1_size,
	   register mp_limb_t s2_limb)
{
  register mp_limb_t x;

  x = *s1_ptr++;
  s2_limb = x + s2_limb;
  *res_ptr++ = s2_limb;
  if (s2_limb < x)
    {
      while (--s1_size != 0)
	{
	  x = *s1_ptr++ + 1;
	  *res_ptr++ = x;
	  if (x != 0)
	    goto fin;
	}

      return 1;
    }

 fin:
  if (res_ptr != s1_ptr)
    {
      mp_size_t i;
      for (i = 0; i < s1_size - 1; i++)
	res_ptr[i] = s1_ptr[i];
    }
  return 0;
}
