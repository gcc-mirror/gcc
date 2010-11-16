/* e_fmodl.c -- long double version of e_fmod.c.
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

/* remainderq(x,p)
 * Return :                  
 * 	returns  x REM p  =  x - [x/p]*p as if in infinite 
 * 	precise arithmetic, where [x/p] is the (infinite bit) 
 *	integer nearest x/p (in half way case choose the even one).
 * Method : 
 *	Based on fmodq() return x-[x/p]chopped*p exactlp.
 */

#include "quadmath-imp.h"

static const __float128 zero = 0.0Q;

__float128
remainderq (__float128 x, __float128 p)
{
  int64_t hx,hp;
  uint64_t sx,lx,lp;
  __float128 p_half;

  GET_FLT128_WORDS64(hx,lx,x);
  GET_FLT128_WORDS64(hp,lp,p);
  sx = hx&0x8000000000000000ULL;
  hp &= 0x7fffffffffffffffLL;
  hx &= 0x7fffffffffffffffLL;

  /* purge off exception values */
  if((hp|lp)==0) return (x*p)/(x*p); 	/* p = 0 */
  if((hx>=0x7fff000000000000LL)||			/* x not finite */
    ((hp>=0x7fff000000000000LL)&&			/* p is NaN */
    (((hp-0x7fff000000000000LL)|lp)!=0)))
      return (x*p)/(x*p);

  if (hp<=0x7ffdffffffffffffLL) x = fmodq (x,p+p);	/* now x < 2p */
  if (((hx-hp)|(lx-lp))==0) return zero*x;
  x  = fabsq(x);
  p  = fabsq(p);
  if (hp<0x0002000000000000LL) {
      if(x+x>p) {
	  x-=p;
	  if(x+x>=p) x -= p;
      }
  } else {
      p_half = 0.5Q*p;
      if(x>p_half) {
	  x-=p;
	  if(x>=p_half) x -= p;
      }
  }
  GET_FLT128_MSW64(hx,x);
  SET_FLT128_MSW64(x,hx^sx);
  return x;
}
