/* Division and remainder routines for Tile.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

typedef int int32_t;
typedef unsigned uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;

/* Raise signal 8 (SIGFPE) with code 1 (FPE_INTDIV).  */
static inline void
raise_intdiv (void)
{
  asm ("{ raise; moveli zero, 8 + (1 << 6) }");
}


#ifndef __tilegx__
/*__udivsi3 - 32 bit integer unsigned divide  */
static inline uint32_t __attribute__ ((always_inline))
__udivsi3_inline (uint32_t dividend, uint32_t divisor)
{
  /* Divide out any power of two factor from dividend and divisor.
     Note that when dividing by zero the divisor will remain zero,
     which is all we need to detect that case below.  */
  const int power_of_two_factor = __insn_ctz (divisor);
  divisor >>= power_of_two_factor;
  dividend >>= power_of_two_factor;

  /* Checks for division by power of two or division by zero.  */
  if (divisor <= 1)
    {
      if (divisor == 0)
	{
	  raise_intdiv ();
	  return 0;
	}
      return dividend;
    }

  /* Compute (a / b) by repeatedly finding the largest N
     such that (b << N) <= a. For each such N, set bit N in the
     quotient, subtract (b << N) from a, and keep going. Think of this as
     the reverse of the "shift-and-add" that a multiply does. The values
     of N are precisely those shift counts.

     Finding N is easy. First, use clz(b) - clz(a) to find the N
     that lines up the high bit of (b << N) with the high bit of a.
     Any larger value of N would definitely make (b << N) > a,
     which is too big.

     Then, if (b << N) > a (because it has larger low bits), decrement
     N by one.  This adjustment will definitely make (b << N) less
     than a, because a's high bit is now one higher than b's.  */

  /* Precomputing the max_ values allows us to avoid a subtract
     in the inner loop and just right shift by clz(remainder).  */
  const int divisor_clz = __insn_clz (divisor);
  const uint32_t max_divisor = divisor << divisor_clz;
  const uint32_t max_qbit = 1 << divisor_clz;

  uint32_t quotient = 0;
  uint32_t remainder = dividend;

  while (remainder >= divisor)
    {
      int shift = __insn_clz (remainder);
      uint32_t scaled_divisor = max_divisor >> shift;
      uint32_t quotient_bit = max_qbit >> shift;

      int too_big = (scaled_divisor > remainder);
      scaled_divisor >>= too_big;
      quotient_bit >>= too_big;
      remainder -= scaled_divisor;
      quotient |= quotient_bit;
    }
  return quotient;
}
#endif /* !__tilegx__ */


/* __udivdi3 - 64 bit integer unsigned divide  */
static inline uint64_t __attribute__ ((always_inline))
__udivdi3_inline (uint64_t dividend, uint64_t divisor)
{
  /* Divide out any power of two factor from dividend and divisor.
     Note that when dividing by zero the divisor will remain zero,
     which is all we need to detect that case below.  */
  const int power_of_two_factor = __builtin_ctzll (divisor);
  divisor >>= power_of_two_factor;
  dividend >>= power_of_two_factor;

  /* Checks for division by power of two or division by zero.  */
  if (divisor <= 1)
    {
      if (divisor == 0)
	{
	  raise_intdiv ();
	  return 0;
	}
      return dividend;
    }

#ifndef __tilegx__
  if (((uint32_t) (dividend >> 32) | ((uint32_t) (divisor >> 32))) == 0)
    {
      /* Operands both fit in 32 bits, so use faster 32 bit algorithm.  */
      return __udivsi3_inline ((uint32_t) dividend, (uint32_t) divisor);
    }
#endif /* !__tilegx__ */

  /* See algorithm description in __udivsi3  */

  const int divisor_clz = __builtin_clzll (divisor);
  const uint64_t max_divisor = divisor << divisor_clz;
  const uint64_t max_qbit = 1ULL << divisor_clz;

  uint64_t quotient = 0;
  uint64_t remainder = dividend;

  while (remainder >= divisor)
    {
      int shift = __builtin_clzll (remainder);
      uint64_t scaled_divisor = max_divisor >> shift;
      uint64_t quotient_bit = max_qbit >> shift;

      int too_big = (scaled_divisor > remainder);
      scaled_divisor >>= too_big;
      quotient_bit >>= too_big;
      remainder -= scaled_divisor;
      quotient |= quotient_bit;
    }
  return quotient;
}


#ifndef __tilegx__
/* __umodsi3 - 32 bit integer unsigned modulo  */
static inline uint32_t __attribute__ ((always_inline))
__umodsi3_inline (uint32_t dividend, uint32_t divisor)
{
  /* Shortcircuit mod by a power of two (and catch mod by zero).  */
  const uint32_t mask = divisor - 1;
  if ((divisor & mask) == 0)
    {
      if (divisor == 0)
	{
	  raise_intdiv ();
	  return 0;
	}
      return dividend & mask;
    }

  /* We compute the remainder (a % b) by repeatedly subtracting off
     multiples of b from a until a < b. The key is that subtracting
     off a multiple of b does not affect the result mod b.

     To make the algorithm run efficiently, we need to subtract
     off a large multiple of b at each step. We subtract the largest
     (b << N) that is <= a.

     Finding N is easy. First, use clz(b) - clz(a) to find the N
     that lines up the high bit of (b << N) with the high bit of a.
     Any larger value of N would definitely make (b << N) > a,
     which is too big.

     Then, if (b << N) > a (because it has larger low bits), decrement
     N by one.  This adjustment will definitely make (b << N) less
     than a, because a's high bit is now one higher than b's.  */
  const uint32_t max_divisor = divisor << __insn_clz (divisor);

  uint32_t remainder = dividend;
  while (remainder >= divisor)
    {
      const int shift = __insn_clz (remainder);
      uint32_t scaled_divisor = max_divisor >> shift;
      scaled_divisor >>= (scaled_divisor > remainder);
      remainder -= scaled_divisor;
    }

  return remainder;
}
#endif /* !__tilegx__ */


/* __umoddi3 - 64 bit integer unsigned modulo  */
static inline uint64_t __attribute__ ((always_inline))
__umoddi3_inline (uint64_t dividend, uint64_t divisor)
{
#ifndef __tilegx__
  if (((uint32_t) (dividend >> 32) | ((uint32_t) (divisor >> 32))) == 0)
    {
      /* Operands both fit in 32 bits, so use faster 32 bit algorithm.  */
      return __umodsi3_inline ((uint32_t) dividend, (uint32_t) divisor);
    }
#endif /* !__tilegx__ */

  /* Shortcircuit mod by a power of two (and catch mod by zero).  */
  const uint64_t mask = divisor - 1;
  if ((divisor & mask) == 0)
    {
      if (divisor == 0)
	{
	  raise_intdiv ();
	  return 0;
	}
      return dividend & mask;
    }

  /* See algorithm description in __umodsi3  */
  const uint64_t max_divisor = divisor << __builtin_clzll (divisor);

  uint64_t remainder = dividend;
  while (remainder >= divisor)
    {
      const int shift = __builtin_clzll (remainder);
      uint64_t scaled_divisor = max_divisor >> shift;
      scaled_divisor >>= (scaled_divisor > remainder);
      remainder -= scaled_divisor;
    }

  return remainder;
}


uint32_t __udivsi3 (uint32_t dividend, uint32_t divisor);
#ifdef L_tile_udivsi3
uint32_t
__udivsi3 (uint32_t dividend, uint32_t divisor)
{
#ifndef __tilegx__
  return __udivsi3_inline (dividend, divisor);
#else /* !__tilegx__ */
  uint64_t n = __udivdi3_inline (((uint64_t) dividend), ((uint64_t) divisor));
  return (uint32_t) n;
#endif /* !__tilegx__ */
}
#endif

#define ABS(x) ((x) >= 0 ? (x) : -(x))

int32_t __divsi3 (int32_t dividend, int32_t divisor);
#ifdef L_tile_divsi3
/* __divsi3 - 32 bit integer signed divide  */
int32_t
__divsi3 (int32_t dividend, int32_t divisor)
{
#ifndef __tilegx__
  uint32_t n = __udivsi3_inline (ABS (dividend), ABS (divisor));
#else /* !__tilegx__ */
  uint64_t n =
    __udivdi3_inline (ABS ((int64_t) dividend), ABS ((int64_t) divisor));
#endif /* !__tilegx__ */
  if ((dividend ^ divisor) < 0)
    n = -n;
  return (int32_t) n;
}
#endif


uint64_t __udivdi3 (uint64_t dividend, uint64_t divisor);
#ifdef L_tile_udivdi3
uint64_t
__udivdi3 (uint64_t dividend, uint64_t divisor)
{
  return __udivdi3_inline (dividend, divisor);
}
#endif

/*__divdi3 - 64 bit integer signed divide  */
int64_t __divdi3 (int64_t dividend, int64_t divisor);
#ifdef L_tile_divdi3
int64_t
__divdi3 (int64_t dividend, int64_t divisor)
{
  uint64_t n = __udivdi3_inline (ABS (dividend), ABS (divisor));
  if ((dividend ^ divisor) < 0)
    n = -n;
  return (int64_t) n;
}
#endif


uint32_t __umodsi3 (uint32_t dividend, uint32_t divisor);
#ifdef L_tile_umodsi3
uint32_t
__umodsi3 (uint32_t dividend, uint32_t divisor)
{
#ifndef __tilegx__
  return __umodsi3_inline (dividend, divisor);
#else /* !__tilegx__ */
  return __umoddi3_inline ((uint64_t) dividend, (uint64_t) divisor);
#endif /* !__tilegx__ */
}
#endif


/* __modsi3 - 32 bit integer signed modulo  */
int32_t __modsi3 (int32_t dividend, int32_t divisor);
#ifdef L_tile_modsi3
int32_t
__modsi3 (int32_t dividend, int32_t divisor)
{
#ifndef __tilegx__
  uint32_t remainder = __umodsi3_inline (ABS (dividend), ABS (divisor));
#else /* !__tilegx__ */
  uint64_t remainder =
    __umoddi3_inline (ABS ((int64_t) dividend), ABS ((int64_t) divisor));
#endif /* !__tilegx__ */
  return (int32_t) ((dividend >= 0) ? remainder : -remainder);
}
#endif


uint64_t __umoddi3 (uint64_t dividend, uint64_t divisor);
#ifdef L_tile_umoddi3
uint64_t
__umoddi3 (uint64_t dividend, uint64_t divisor)
{
  return __umoddi3_inline (dividend, divisor);
}
#endif


/* __moddi3 - 64 bit integer signed modulo  */
int64_t __moddi3 (int64_t dividend, int64_t divisor);
#ifdef L_tile_moddi3
int64_t
__moddi3 (int64_t dividend, int64_t divisor)
{
  uint64_t remainder = __umoddi3_inline (ABS (dividend), ABS (divisor));
  return (int64_t) ((dividend >= 0) ? remainder : -remainder);
}
#endif
