/* s_copysignl.c -- long double version of s_copysign.c.
 * Conversion to long double by Jakub Jelinek, jj@ultra.linux.cz.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

#include "quadmath-imp.h"

__float128
copysignq (__float128 x, __float128 y)
{
  uint64_t hx,hy;
  GET_FLT128_MSW64(hx,x);
  GET_FLT128_MSW64(hy,y);
  SET_FLT128_MSW64(x,(hx&0x7fffffffffffffffULL)|(hy&0x8000000000000000ULL));
  return x;
}
