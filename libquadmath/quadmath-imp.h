/* GCC Quad-Precision Math Library
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.
   Written by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>

This file is part of the libquadmath library.
Libquadmath is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libquadmath is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libquadmath; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef QUADMATH_IMP_H
#define QUADMATH_IMP_H

#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "quadmath.h"
#include "config.h"
#ifdef HAVE_FENV_H
# include <fenv.h>
#endif


/* Under IEEE 754, an architecture may determine tininess of
   floating-point results either "before rounding" or "after
   rounding", but must do so in the same way for all operations
   returning binary results.  Define TININESS_AFTER_ROUNDING to 1 for
   "after rounding" architectures, 0 for "before rounding"
   architectures.  */

#define TININESS_AFTER_ROUNDING   1

#define HIGH_ORDER_BIT_IS_SET_FOR_SNAN 0

#define FIX_FLT128_LONG_CONVERT_OVERFLOW 0
#define FIX_FLT128_LLONG_CONVERT_OVERFLOW 0

/* Prototypes for internal functions.  */
extern int32_t __quadmath_rem_pio2q (__float128, __float128 *);
extern void __quadmath_kernel_sincosq (__float128, __float128, __float128 *,
				       __float128 *, int);
extern __float128 __quadmath_kernel_sinq (__float128, __float128, int);
extern __float128 __quadmath_kernel_cosq (__float128, __float128);
extern __float128 __quadmath_kernel_tanq (__float128, __float128, int);
extern __float128 __quadmath_gamma_productq (__float128, __float128, int,
					     __float128 *);
extern __float128 __quadmath_gammaq_r (__float128, int *);
extern __float128 __quadmath_lgamma_negq (__float128, int *);
extern __float128 __quadmath_lgamma_productq (__float128, __float128,
					      __float128, int);
extern __float128 __quadmath_lgammaq_r (__float128, int *);
extern __float128 __quadmath_x2y2m1q (__float128 x, __float128 y);
extern __complex128 __quadmath_kernel_casinhq (__complex128, int);

static inline void
mul_splitq (__float128 *hi, __float128 *lo, __float128 x, __float128 y)
{
  /* Fast built-in fused multiply-add.  */
  *hi = x * y;
  *lo = fmaq (x, y, -*hi);
}




/* Frankly, if you have __float128, you have 64-bit integers, right?  */
#ifndef UINT64_C
# error "No way!"
#endif


/* Main union type we use to manipulate the floating-point type.  */
typedef union
{
  __float128 value;

  struct
#ifdef __MINGW32__
  /* On mingw targets the ms-bitfields option is active by default.
     Therefore enforce gnu-bitfield style.  */
  __attribute__ ((gcc_struct))
#endif
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned mantissa0:16;
    unsigned mantissa1:32;
    unsigned mantissa2:32;
    unsigned mantissa3:32;
#else
    unsigned mantissa3:32;
    unsigned mantissa2:32;
    unsigned mantissa1:32;
    unsigned mantissa0:16;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } ieee;

  struct
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint64_t high;
    uint64_t low;
#else
    uint64_t low;
    uint64_t high;
#endif
  } words64;

  struct
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint32_t w0;
    uint32_t w1;
    uint32_t w2;
    uint32_t w3;
#else
    uint32_t w3;
    uint32_t w2;
    uint32_t w1;
    uint32_t w0;
#endif
  } words32;

  struct
#ifdef __MINGW32__
  /* Make sure we are using gnu-style bitfield handling.  */
  __attribute__ ((gcc_struct))
#endif
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned quiet_nan:1;
    unsigned mantissa0:15;
    unsigned mantissa1:32;
    unsigned mantissa2:32;
    unsigned mantissa3:32;
#else
    unsigned mantissa3:32;
    unsigned mantissa2:32;
    unsigned mantissa1:32;
    unsigned mantissa0:15;
    unsigned quiet_nan:1;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } ieee_nan;

} ieee854_float128;


/* Get two 64 bit ints from a long double.  */
#define GET_FLT128_WORDS64(ix0,ix1,d)  \
do {                                   \
  ieee854_float128 u;                  \
  u.value = (d);                       \
  (ix0) = u.words64.high;              \
  (ix1) = u.words64.low;               \
} while (0)

/* Set a long double from two 64 bit ints.  */
#define SET_FLT128_WORDS64(d,ix0,ix1)  \
do {                                   \
  ieee854_float128 u;                  \
  u.words64.high = (ix0);              \
  u.words64.low = (ix1);               \
  (d) = u.value;                       \
} while (0)

/* Get the more significant 64 bits of a long double mantissa.  */
#define GET_FLT128_MSW64(v,d)          \
do {                                   \
  ieee854_float128 u;                  \
  u.value = (d);                       \
  (v) = u.words64.high;                \
} while (0)

/* Set the more significant 64 bits of a long double mantissa from an int.  */
#define SET_FLT128_MSW64(d,v)          \
do {                                   \
  ieee854_float128 u;                  \
  u.value = (d);                       \
  u.words64.high = (v);                \
  (d) = u.value;                       \
} while (0)

/* Get the least significant 64 bits of a long double mantissa.  */
#define GET_FLT128_LSW64(v,d)          \
do {                                   \
  ieee854_float128 u;                  \
  u.value = (d);                       \
  (v) = u.words64.low;                 \
} while (0)


#define IEEE854_FLOAT128_BIAS 0x3fff

#define QUADFP_NAN		0
#define QUADFP_INFINITE		1
#define QUADFP_ZERO		2
#define QUADFP_SUBNORMAL	3
#define QUADFP_NORMAL		4
#define fpclassifyq(x) \
  __builtin_fpclassify (QUADFP_NAN, QUADFP_INFINITE, QUADFP_NORMAL, \
			QUADFP_SUBNORMAL, QUADFP_ZERO, x)

#ifndef math_opt_barrier
# define math_opt_barrier(x) \
({ __typeof (x) __x = (x); __asm ("" : "+m" (__x)); __x; })
# define math_force_eval(x) \
({ __typeof (x) __x = (x); __asm __volatile__ ("" : : "m" (__x)); })
#endif

/* math_narrow_eval reduces its floating-point argument to the range
   and precision of its semantic type.  (The original evaluation may
   still occur with excess range and precision, so the result may be
   affected by double rounding.)  */
#define math_narrow_eval(x) (x)

/* If X (which is not a NaN) is subnormal, force an underflow
   exception.  */
#define math_check_force_underflow(x)				\
  do								\
    {								\
      __float128 force_underflow_tmp = (x);			\
      if (fabsq (force_underflow_tmp) < FLT128_MIN)		\
	{							\
	  __float128 force_underflow_tmp2			\
	    = force_underflow_tmp * force_underflow_tmp;	\
	  math_force_eval (force_underflow_tmp2);		\
	}							\
    }								\
  while (0)
/* Likewise, but X is also known to be nonnegative.  */
#define math_check_force_underflow_nonneg(x)			\
  do								\
    {								\
      __float128 force_underflow_tmp = (x);			\
      if (force_underflow_tmp < FLT128_MIN)			\
	{							\
	  __float128 force_underflow_tmp2			\
	    = force_underflow_tmp * force_underflow_tmp;	\
	  math_force_eval (force_underflow_tmp2);		\
	}							\
    }								\
  while (0)

/* Likewise, for both real and imaginary parts of a complex
   result.  */
#define math_check_force_underflow_complex(x)				\
  do									\
    {									\
      __typeof (x) force_underflow_complex_tmp = (x);			\
      math_check_force_underflow (__real__ force_underflow_complex_tmp); \
      math_check_force_underflow (__imag__ force_underflow_complex_tmp); \
    }									\
  while (0)

#ifndef HAVE_FENV_H
# define feraiseexcept(arg) ((void) 0)
typedef int fenv_t;
# define feholdexcept(arg) ((void) 0)
# define fesetround(arg) ((void) 0)
# define feupdateenv(arg) ((void) (arg))
# define fesetenv(arg) ((void) (arg))
# define fetestexcept(arg) 0
# define feclearexcept(arg) ((void) 0)
#else
# ifndef HAVE_FEHOLDEXCEPT
#  define feholdexcept(arg) ((void) 0)
# endif
# ifndef HAVE_FESETROUND
#  define fesetround(arg) ((void) 0)
# endif
# ifndef HAVE_FEUPDATEENV
#  define feupdateenv(arg) ((void) (arg))
# endif
# ifndef HAVE_FESETENV
#  define fesetenv(arg) ((void) (arg))
# endif
# ifndef HAVE_FETESTEXCEPT
#  define fetestexcept(arg) 0
# endif
#endif

#ifndef __glibc_likely
# define __glibc_likely(cond)	__builtin_expect ((cond), 1)
#endif

#ifndef __glibc_unlikely
# define __glibc_unlikely(cond)	__builtin_expect ((cond), 0)
#endif

#if defined HAVE_FENV_H && defined HAVE_FESETROUND && defined HAVE_FEUPDATEENV
struct rm_ctx
{
  fenv_t env;
  bool updated_status;
};

# define SET_RESTORE_ROUNDF128(RM)					\
  struct rm_ctx ctx __attribute__((cleanup (libc_feresetround_ctx)));	\
  libc_feholdsetround_ctx (&ctx, (RM))

static inline __attribute__ ((always_inline)) void
libc_feholdsetround_ctx (struct rm_ctx *ctx, int round)
{
  ctx->updated_status = false;

  /* Update rounding mode only if different.  */
  if (__glibc_unlikely (round != fegetround ()))
    {
      ctx->updated_status = true;
      fegetenv (&ctx->env);
      fesetround (round);
    }
}

static inline __attribute__ ((always_inline)) void
libc_feresetround_ctx (struct rm_ctx *ctx)
{
  /* Restore the rounding mode if updated.  */
  if (__glibc_unlikely (ctx->updated_status))
    feupdateenv (&ctx->env);
}
#else
# define SET_RESTORE_ROUNDF128(RM) ((void) 0)
#endif

#endif
