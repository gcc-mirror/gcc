/* Signed and unsigned multiplication and division and modulus for CRIS.
   Contributed by Axis Communications.
   Written by Hans-Peter Nilsson <hp@axis.se>, c:a 1992.

   Copyright (C) 1998-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
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


/* Note that we provide prototypes for all "const" functions, to attach
   the const attribute.  This is necessary in 2.7.2 - adding the
   attribute to the function *definition* is a syntax error.
    This did not work with e.g. 2.1; back then, the return type had to
   be "const".  */

#include "config.h"

#if defined (__CRIS_arch_version) && __CRIS_arch_version >= 3
#define LZ(v) __builtin_clz (v)
#endif

/* In (at least) the 4.7 series, GCC doesn't automatically choose the
   most optimal strategy, possibly related to insufficient modelling of
   delay-slot costs.  */
#if defined (__CRIS_arch_version) && __CRIS_arch_version >= 10
#define SIGNMULT(s, a) ((s) * (a)) /* Cheap multiplication, better than branch.  */
#else
#define SIGNMULT(s, a) ((s) < 0 ? -(a) : (a)) /* Branches are still better.  */
#endif

#if defined (L_udivsi3) || defined (L_divsi3) || defined (L_umodsi3) \
    || defined (L_modsi3)
/* Result type of divmod worker function.  */
struct quot_rem
 {
   long quot;
   long rem;
 };

/* This is the worker function for div and mod.  It is inlined into the
   respective library function.  Parameter A must have bit 31 == 0.  */

static __inline__ struct quot_rem
do_31div (unsigned long a, unsigned long b)
     __attribute__ ((__const__, __always_inline__));

static __inline__ struct quot_rem
do_31div (unsigned long a, unsigned long b)
{
  /* Adjust operands and result if a is 31 bits.  */
  long extra = 0;
  int quot_digits = 0;

  if (b == 0)
    {
      struct quot_rem ret;
      ret.quot = 0xffffffff;
      ret.rem = 0xffffffff;
      return ret;
    }

  if (a < b)
    return (struct quot_rem) { 0, a };

#ifdef LZ
  if (b <= a)
    {
      quot_digits = LZ (b) - LZ (a);
      quot_digits += (a >= (b << quot_digits));
      b <<= quot_digits;
    }
#else
  while (b <= a)
    {
      b <<= 1;
      quot_digits++;
    }
#endif

  /* Is a 31 bits?  Note that bit 31 is handled by the caller.  */
  if (a & 0x40000000)
    {
      /* Then make b:s highest bit max 0x40000000, because it must have
	 been 0x80000000 to be 1 bit higher than a.  */
      b >>= 1;

      /* Adjust a to be maximum 0x3fffffff, i.e. two upper bits zero.  */
      if (a >= b)
	{
	  a -= b;
	  extra = 1 << (quot_digits - 1);
	}
      else
	{
	  a -= b >> 1;

	  /* Remember that we adjusted a by subtracting b * 2 ** Something.  */
	  extra = 1 << quot_digits;
	}

      /* The number of quotient digits will be one less, because
	 we just adjusted b.  */
      quot_digits--;
    }

  /* Now do the division part.  */

  /* Subtract b and add ones to the right when a >= b
     i.e. "a - (b - 1) == (a - b) + 1".  */
  b--;

#define DS __asm__ ("dstep %2,%0" : "=r" (a) : "0" (a), "r" (b)); \
 __attribute__ ((__fallthrough__))

  switch (quot_digits)
    {
    case 32: DS; case 31: DS; case 30: DS; case 29: DS;
    case 28: DS; case 27: DS; case 26: DS; case 25: DS;
    case 24: DS; case 23: DS; case 22: DS; case 21: DS;
    case 20: DS; case 19: DS; case 18: DS; case 17: DS;
    case 16: DS; case 15: DS; case 14: DS; case 13: DS;
    case 12: DS; case 11: DS; case 10: DS; case 9: DS;
    case 8: DS; case 7: DS; case 6: DS; case 5: DS;
    case 4: DS; case 3: DS; case 2: DS; case 1: DS;
    case 0:;
    }

  {
    struct quot_rem ret;
    ret.quot = (a & ((1 << quot_digits) - 1)) + extra;
    ret.rem = a >> quot_digits;
    return ret;
  }
}

#ifdef L_udivsi3
unsigned long
__Udiv (unsigned long a, unsigned long b) __attribute__ ((__const__));

unsigned long
__Udiv (unsigned long a, unsigned long b)
{
  long extra = 0;

  /* Adjust operands and result, if a and/or b is 32 bits.  */
  /* Effectively: b & 0x80000000.  */
  if ((long) b < 0)
    return a >= b;

  /* Effectively: a & 0x80000000.  */
  if ((long) a < 0)
    {
      int tmp = 0;

      if (b == 0)
	return 0xffffffff;
#ifdef LZ
      tmp = LZ (b);
#else
      for (tmp = 31; (((long) b & (1 << tmp)) == 0); tmp--)
	;

      tmp = 31 - tmp;
#endif

      if ((b << tmp) > a)
	{
	  extra = 1 << (tmp-1);
	  a -= b << (tmp - 1);
	}
      else
	{
	  extra = 1 << tmp;
	  a -= b << tmp;
	}
    }

  return do_31div (a, b).quot+extra;
}
#endif /* L_udivsi3 */

#ifdef L_divsi3
long
__Div (long a, long b) __attribute__ ((__const__));

long
__Div (long a, long b)
{
  long extra = 0;
  long sign = (b < 0) ? -1 : 1;
  long res;

  /* We need to handle a == -2147483648 as expected and must while
     doing that avoid producing a sequence like "abs (a) < 0" as GCC
     may optimize out the test.  That sequence may not be obvious as
     we call inline functions.  Testing for a being negative and
     handling (presumably much rarer than positive) enables us to get
     a bit of optimization for an (accumulated) reduction of the
     penalty of the 0x80000000 special-case.  */
  if (a < 0)
    {
      sign = -sign;

      if ((a & 0x7fffffff) == 0)
	{
	  /* We're at 0x80000000.  Tread carefully.  */
	  a -= SIGNMULT (sign, b);
	  extra = sign;
	}
      a = -a;
    }

  res = do_31div (a, __builtin_labs (b)).quot;
  return SIGNMULT (sign, res) + extra;
}
#endif /* L_divsi3 */


#ifdef L_umodsi3
unsigned long
__Umod (unsigned long a, unsigned long b) __attribute__ ((__const__));

unsigned long
__Umod (unsigned long a, unsigned long b)
{
  /* Adjust operands and result if a and/or b is 32 bits.  */
  if ((long) b < 0)
    return a >= b ? a - b : a;

  if ((long) a < 0)
    {
      int tmp = 0;

      if (b == 0)
	return a;
#ifdef LZ
      tmp = LZ (b);
#else
      for (tmp = 31; (((long) b & (1 << tmp)) == 0); tmp--)
	;
      tmp = 31 - tmp;
#endif

      if ((b << tmp) > a)
	{
	  a -= b << (tmp - 1);
	}
      else
	{
	  a -= b << tmp;
	}
    }

  return do_31div (a, b).rem;
}
#endif /* L_umodsi3 */

#ifdef L_modsi3
long
__Mod (long a, long b) __attribute__ ((__const__));

long
__Mod (long a, long b)
{
  long sign = 1;
  long res;

  /* We need to handle a == -2147483648 as expected and must while
     doing that avoid producing a sequence like "abs (a) < 0" as GCC
     may optimize out the test.  That sequence may not be obvious as
     we call inline functions.  Testing for a being negative and
     handling (presumably much rarer than positive) enables us to get
     a bit of optimization for an (accumulated) reduction of the
     penalty of the 0x80000000 special-case.  */
  if (a < 0)
    {
      sign = -1;
      if ((a & 0x7fffffff) == 0)
	/* We're at 0x80000000.  Tread carefully.  */
	a += __builtin_labs (b);
      a = -a;
    }

  res = do_31div (a, __builtin_labs (b)).rem;
  return SIGNMULT (sign, res);
}
#endif /* L_modsi3 */
#endif /* L_udivsi3 || L_divsi3 || L_umodsi3 || L_modsi3 */

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
