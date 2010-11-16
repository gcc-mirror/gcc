/*
 * Written by J.T. Conklin <jtc@netbsd.org>.
 * Change for long double by Jakub Jelinek <jj@ultra.linux.cz>
 * Public domain.
 */

#include "quadmath-imp.h"

int
isinfq (__float128 x)
{
  int64_t hx,lx;
  GET_FLT128_WORDS64(hx,lx,x);
  lx |= (hx & 0x7fffffffffffffffLL) ^ 0x7fff000000000000LL;
  lx |= -lx;
  return ~(lx >> 63) & (hx >> 62);
}
