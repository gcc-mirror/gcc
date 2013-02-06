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

#include <stdint.h>
#include <stdlib.h>
#include "quadmath.h"
#include "config.h"


/* Under IEEE 754, an architecture may determine tininess of
   floating-point results either "before rounding" or "after
   rounding", but must do so in the same way for all operations
   returning binary results.  Define TININESS_AFTER_ROUNDING to 1 for
   "after rounding" architectures, 0 for "before rounding"
   architectures.  */

#define TININESS_AFTER_ROUNDING   1


/* Prototypes for internal functions.  */
extern int32_t __quadmath_rem_pio2q (__float128, __float128 *);
extern void __quadmath_kernel_sincosq (__float128, __float128, __float128 *,
				       __float128 *, int);
extern __float128 __quadmath_kernel_sinq (__float128, __float128, int);
extern __float128 __quadmath_kernel_cosq (__float128, __float128);
extern __float128 __quadmath_x2y2m1q (__float128 x, __float128 y);
extern int __quadmath_isinf_nsq (__float128 x);





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
    uint64_t mant_high:48;
    uint64_t mant_low:64;
#else
    uint64_t mant_low:64;
    uint64_t mant_high:48;
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
    uint64_t mant_high:47;
    uint64_t mant_low:64;
#else
    uint64_t mant_low:64;
    uint64_t mant_high:47;
    unsigned quiet_nan:1;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } nan;

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

#endif
