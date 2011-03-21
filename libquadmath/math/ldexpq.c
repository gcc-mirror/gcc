/* s_ldexpl.c -- long double version of s_ldexp.c.
 * Conversion to long double by Ulrich Drepper,
 * Cygnus Support, drepper@cygnus.com.
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

#include <errno.h>
#include "quadmath-imp.h"

__float128
ldexpq (__float128 value, int exp)
{
  if(!finiteq(value)||value==0.0Q) return value;
  value = scalbnq(value,exp);
  if(!finiteq(value)||value==0.0Q) errno = ERANGE;
  return value;
}
