/* Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

/* 32-bit V64SI divide and modulo as used in gcn.
   This is a simple conversion from lib2-divmod.c.  */

#define MASKMODE v64si
#include "amdgcn_veclib.h"

static v64udi
__udivmodv64si4_aux (v64usi num, v64usi den, v64si __mask)
{
  v64usi bit = VECTOR_INIT (1U);
  v64usi res = VECTOR_INIT (0U);

  VECTOR_WHILE ((den < num) & (bit != 0) & ((den & (1L<<31)) == 0),
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
__divmodv64si4_aux (v64si a, v64si b, v64si __mask)
{
  v64si nega = VECTOR_INIT (0);
  v64si negb = VECTOR_INIT (0);

  VECTOR_IF (a < 0, cond)
    VECTOR_COND_MOVE (a, -a, cond);
    nega = cond;
  VECTOR_ENDIF

  VECTOR_IF (b < 0, cond)
    VECTOR_COND_MOVE (b, -b, cond);
    negb = cond;
  VECTOR_ENDIF

  v64usi ua = __builtin_convertvector (a, v64usi);
  v64usi ub = __builtin_convertvector (b, v64usi);
  v64udi pair = __udivmodv64si4_aux (ua, ub, __mask);

  v64si quot = UNPACK_SI_LOW (v64si, pair);
  v64si rem = UNPACK_SI_HIGH (v64si, pair);
  VECTOR_COND_MOVE (quot, -quot, nega ^ negb);
  VECTOR_COND_MOVE (rem, -rem, nega);
  pair = PACK_SI_PAIR (quot, rem);

  return pair;
}


static inline v64si
__divv64si3_aux (v64si a, v64si b, v64si __mask)
{
  v64udi pair = __divmodv64si4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64si, pair);
}

static inline v64si
__modv64si3_aux (v64si a, v64si b, v64si __mask)
{
  v64udi pair = __divmodv64si4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64si, pair);
}


static inline v64usi
__udivv64si3_aux (v64usi a, v64usi b, v64si __mask)
{
  v64udi pair = __udivmodv64si4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64usi, pair);
}

static inline v64usi
__umodv64si3_aux (v64usi a, v64usi b, v64si __mask)
{
  v64udi pair = __udivmodv64si4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64usi, pair);
}

DEF_VARIANTS (__div, si3, si)
DEF_VARIANTS (__mod, si3, si)
DEF_VARIANTS_B (__divmod, si4, udi, si)
DEF_VARIANTS (__udiv, si3, usi)
DEF_VARIANTS (__umod, si3, usi)
DEF_VARIANTS_B (__udivmod, si4, udi, usi)
