/* Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Contributed by Altera and Mentor Graphics, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "lib2-gcn.h"

/* 16-bit V64HI divide and modulo as used in gcn.
   This is a simple conversion from lib2-divmod.c.  */

#define MASKMODE v64hi
#include "amdgcn_veclib.h"

static v64udi
__udivmodv64hi4_aux (v64uhi num, v64uhi den, v64hi __mask)
{
  v64uhi bit = VECTOR_INIT ((unsigned short)1U);
  v64uhi res = VECTOR_INIT ((unsigned short)0U);

  VECTOR_WHILE ((den < num) & (bit != 0) & ((den & (1L<<15)) == 0),
		cond, NO_COND)
    VECTOR_COND_MOVE (den, den << 1, cond);
    VECTOR_COND_MOVE (bit, bit << 1, cond);
  VECTOR_ENDWHILE
  VECTOR_WHILE (bit != 0, loopcond, NO_COND)
    VECTOR_IF2 (num >= den, ifcond, loopcond)
      VECTOR_COND_MOVE (num, num - den, ifcond);
      VECTOR_COND_MOVE (res, res | bit, ifcond);
    VECTOR_ENDIF
    VECTOR_COND_MOVE (bit, bit >> 1, loopcond);
    VECTOR_COND_MOVE (den, den >> 1, loopcond);
  VECTOR_ENDWHILE

  return PACK_SI_PAIR (res, num);
}

static v64udi
__divmodv64hi4_aux (v64hi a, v64hi b,  v64hi __mask)
{
  v64hi nega = VECTOR_INIT ((short)0);
  v64hi negb = VECTOR_INIT ((short)0);

  VECTOR_IF (a < 0, cond)
    VECTOR_COND_MOVE (a, -a, cond);
    nega = cond;
  VECTOR_ENDIF

  VECTOR_IF (b < 0, cond)
    VECTOR_COND_MOVE (b, -b, cond);
    negb = cond;
  VECTOR_ENDIF

  v64uhi ua = __builtin_convertvector (a, v64uhi);
  v64uhi ub = __builtin_convertvector (b, v64uhi);
  v64udi pair = __udivmodv64hi4_aux (ua, ub, __mask);

  v64hi quot = UNPACK_SI_LOW (v64hi, pair);
  v64hi rem = UNPACK_SI_HIGH (v64hi, pair);
  VECTOR_COND_MOVE (quot, -quot, nega ^ negb);
  VECTOR_COND_MOVE (rem, -rem, nega);
  pair = PACK_SI_PAIR (quot, rem);

  return pair;
}


static inline v64hi
__divv64hi3_aux (v64hi a, v64hi b, v64hi __mask)
{
  v64udi pair = __divmodv64hi4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64hi, pair);
}

static inline v64hi
__modv64hi3_aux (v64hi a, v64hi b, v64hi __mask)
{
  v64udi pair = __divmodv64hi4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64hi, pair);
}


static inline v64uhi
__udivv64hi3_aux (v64uhi a, v64uhi b, v64hi __mask)
{
  v64udi pair = __udivmodv64hi4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64uhi, pair);
}

static inline v64uhi
__umodv64hi3_aux (v64uhi a, v64uhi b, v64hi __mask)
{
  v64udi pair = __udivmodv64hi4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64uhi, pair);
}

DEF_VARIANTS (__div, hi3, hi)
DEF_VARIANTS (__mod, hi3, hi)
DEF_VARIANTS_B (__divmod, hi4, udi, hi)
DEF_VARIANTS (__udiv, hi3, uhi)
DEF_VARIANTS (__umod, hi3, uhi)
DEF_VARIANTS_B (__udivmod, hi4, udi, uhi)
