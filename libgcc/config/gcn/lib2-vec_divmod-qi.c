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

/* 8-bit V64QI divide and modulo as used in gcn.
   This is a simple conversion from lib2-divmod.c.  */

#define MASKMODE v64qi
#include "amdgcn_veclib.h"

static v64udi
__udivmodv64qi4_aux (v64uqi num, v64uqi den, v64qi __mask)
{
  v64uqi bit = VECTOR_INIT ((unsigned char)1U);
  v64uqi res = VECTOR_INIT ((unsigned char)0U);

  VECTOR_WHILE ((den < num) & (bit != 0) & ((den & (1<<7)) == 0),
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
__divmodv64qi4_aux (v64qi a, v64qi b, v64qi __mask)
{
  v64qi nega = VECTOR_INIT ((char)0);
  v64qi negb = VECTOR_INIT ((char)0);

  VECTOR_IF (a < 0, cond)
    VECTOR_COND_MOVE (a, -a, cond);
    nega = cond;
  VECTOR_ENDIF

  VECTOR_IF (b < 0, cond)
    VECTOR_COND_MOVE (b, -b, cond);
    negb = cond;
  VECTOR_ENDIF

  v64uqi ua = __builtin_convertvector (a, v64uqi);
  v64uqi ub = __builtin_convertvector (b, v64uqi);
  v64udi pair = __udivmodv64qi4_aux (ua, ub, __mask);

  v64qi quot = UNPACK_SI_LOW (v64qi, pair);
  v64qi rem = UNPACK_SI_HIGH (v64qi, pair);
  VECTOR_COND_MOVE (quot, -quot, nega ^ negb);
  VECTOR_COND_MOVE (rem, -rem, nega);
  pair = PACK_SI_PAIR (quot, rem);

  return pair;
}


static inline v64qi
__divv64qi3_aux (v64qi a, v64qi b, v64qi __mask)
{
  v64udi pair = __divmodv64qi4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64qi, pair);
}

static inline v64qi
__modv64qi3_aux (v64qi a, v64qi b, v64qi __mask)
{
  v64udi pair = __divmodv64qi4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64qi, pair);
}


static inline v64uqi
__udivv64qi3_aux (v64uqi a, v64uqi b, v64qi __mask)
{
  v64udi pair = __udivmodv64qi4_aux (a, b, __mask);
  return UNPACK_SI_LOW (v64uqi, pair);
}

static inline v64uqi
__umodv64qi3_aux (v64uqi a, v64uqi b, v64qi __mask)
{
  v64udi pair = __udivmodv64qi4_aux (a, b, __mask);
  return UNPACK_SI_HIGH (v64uqi, pair);
}

DEF_VARIANTS (__div, qi3, qi)
DEF_VARIANTS (__mod, qi3, qi)
DEF_VARIANTS_B (__divmod, qi4, udi, qi)
DEF_VARIANTS (__udiv, qi3, uqi)
DEF_VARIANTS (__umod, qi3, uqi)
DEF_VARIANTS_B (__udivmod, qi4, udi, uqi)
