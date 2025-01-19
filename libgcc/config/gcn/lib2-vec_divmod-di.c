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

/* 64-bit V64SI divide and modulo as used in gcn.
   This is a simple conversion from lib2-divmod.c.  */

#define MASKMODE v64di
#include "amdgcn_veclib.h"

static v64uti
__udivmodv64di4_aux (v64udi num, v64udi den, v64di __mask)
{
  v64udi bit = VECTOR_INIT (1UL);
  v64udi res = VECTOR_INIT (0UL);

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

  return PACK_DI_PAIR (res, num);
}

static v64uti
__divmodv64di4_aux (v64di a, v64di b, v64di __mask)
{
  v64di nega = VECTOR_INIT (0L);
  v64di negb = VECTOR_INIT (0L);

  VECTOR_IF (a < 0, cond)
    VECTOR_COND_MOVE (a, -a, cond);
    nega = cond;
  VECTOR_ENDIF

  VECTOR_IF (b < 0, cond)
    VECTOR_COND_MOVE (b, -b, cond);
    negb = cond;
  VECTOR_ENDIF

  v64udi ua = __builtin_convertvector (a, v64udi);
  v64udi ub = __builtin_convertvector (b, v64udi);
  v64uti pair = __udivmodv64di4_aux (ua, ub, __mask);

  v64di quot = UNPACK_DI_LOW (v64di, pair);
  v64di rem = UNPACK_DI_HIGH (v64di, pair);
  VECTOR_COND_MOVE (quot, -quot, nega ^ negb);
  VECTOR_COND_MOVE (rem, -rem, nega);
  pair = PACK_DI_PAIR (quot, rem);

  return pair;
}


static inline v64di
__divv64di3_aux (v64di a, v64di b, v64di __mask)
{
  v64uti pair = __divmodv64di4_aux (a, b, __mask);
  return UNPACK_DI_LOW (v64di, pair);
}

static inline v64di
__modv64di3_aux (v64di a, v64di b, v64di __mask)
{
  v64uti pair = __divmodv64di4_aux (a, b, __mask);
  return UNPACK_DI_HIGH (v64di, pair);
}


static inline v64udi
__udivv64di3_aux (v64udi a, v64udi b, v64di __mask)
{
  v64uti pair = __udivmodv64di4_aux (a, b, __mask);
  return UNPACK_DI_LOW (v64udi, pair);
}

static inline v64udi
__umodv64di3_aux (v64udi a, v64udi b, v64di __mask)
{
  v64uti pair = __udivmodv64di4_aux (a, b, __mask);
  return UNPACK_DI_HIGH (v64udi, pair);
}

DEF_VARIANTS (__div, di3, di)
DEF_VARIANTS (__mod, di3, di)
DEF_VARIANTS_B (__divmod, di4, uti, di)
DEF_VARIANTS (__udiv, di3, udi)
DEF_VARIANTS (__umod, di3, udi)
DEF_VARIANTS_B (__udivmod, di4, uti, udi)
