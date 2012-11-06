/*
 * Written by Ulrich Drepper <drepper@gmail.com>
 */

/*
 * __quadmath_isinf_nsq (x) returns != 0 if x is ±inf, else 0;
 * no branching!
 */

#include "quadmath-imp.h"

int
__quadmath_isinf_nsq (__float128 x)
{
        int64_t hx,lx;
        GET_FLT128_WORDS64(hx,lx,x);
        return !(lx | ((hx & 0x7fffffffffffffffLL) ^ 0x7fff000000000000LL));
}

