/* finiteq.c -- __float128 version of s_finite.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
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

/*
 * finiteq(x) returns 1 is x is finite, else 0;
 * no branching!
 */

int
finiteq (const __float128 x)
{
  int64_t hx;
  GET_FLT128_MSW64(hx,x);
  return (int)((uint64_t)((hx&0x7fff000000000000LL)
			   -0x7fff000000000000LL)>>63);
}
