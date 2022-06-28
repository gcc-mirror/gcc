/* Fallback implementation of issignaling macro.
   Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"

/* This header provides an implementation of the type-generic issignaling macro.
   Some points of note:

     - This header is only included if the issignaling macro is not defined.
     - All targets for which Fortran IEEE modules are supported currently have
       the high-order bit of the NaN mantissa clear for signaling (and set
       for quiet), as recommended by IEEE.
     - We use the __*_IS_IEC_60559__ macros to make sure we only deal with formats
       we know. For other floating-point formats, we consider all NaNs as quiet.

 */

typedef union
{
  float value;
  uint32_t word;
} ieee_float_shape_type;

static inline int
__issignalingf (float x)
{
#if __FLT_IS_IEC_60559__
  uint32_t xi;
  ieee_float_shape_type u;

  u.value = x;
  xi = u.word;

  xi ^= 0x00400000;
  return (xi & 0x7fffffff) > 0x7fc00000;
#else
  return 0;
#endif
}


typedef union
{
  double value;
  uint64_t word;
} ieee_double_shape_type;

static inline int
__issignaling (double x)
{
#if __DBL_IS_IEC_60559__
  ieee_double_shape_type u;
  uint64_t xi;

  u.value = x;
  xi = u.word;

  xi ^= UINT64_C (0x0008000000000000);
  return (xi & UINT64_C (0x7fffffffffffffff)) > UINT64_C (0x7ff8000000000000);
#else
  return 0;
#endif
}


#if __LDBL_DIG__ == __DBL_DIG__

/* Long double is the same as double.  */
static inline int
__issignalingl (long double x)
{
  return __issignaling (x);
}

#elif (__LDBL_DIG__ == 18) && __LDBL_IS_IEC_60559__

/* Long double is x86 extended type.  */

typedef union
{
  long double value;
  struct
  {
#if __FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__
    int sign_exponent:16;
    unsigned int empty:16;
    uint32_t msw;
    uint32_t lsw;
#elif __FLOAT_WORD_ORDER__ == __ORDER_LITTLE_ENDIAN__
    uint32_t lsw;
    uint32_t msw;
    int sign_exponent:16;
    unsigned int empty:16;
#endif
  } parts;
} ieee_long_double_shape_type;

static inline int
__issignalingl (long double x)
{
  int ret;
  uint32_t exi, hxi, lxi;
  ieee_long_double_shape_type u;

  u.value = x;
  exi = u.parts.sign_exponent;
  hxi = u.parts.msw;
  lxi = u.parts.lsw;

  /* Pseudo numbers on x86 are always signaling.  */
  ret = (exi & 0x7fff) && ((hxi & 0x80000000) == 0);

  hxi ^= 0x40000000;
  hxi |= (lxi | -lxi) >> 31;
  return ret || (((exi & 0x7fff) == 0x7fff) && (hxi > 0xc0000000));
}

#elif (__LDBL_DIG__ == 31)

/* Long double is 128-bit IBM extended type.  */

static inline int
__issignalingl (long double x)
{
  union { long double value; double parts[2]; } u;

  u.value = x;
  return __issignaling (u.parts[0]);
}

#elif (__LDBL_DIG__ == 33) && __LDBL_IS_IEC_60559__

/* Long double is 128-bit type.  */

typedef union
{
  long double value;
  struct
  {
#if __FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__
    uint64_t msw;
    uint64_t lsw;
#elif __FLOAT_WORD_ORDER__ == __ORDER_LITTLE_ENDIAN__
    uint64_t lsw;
    uint64_t msw;
#endif
  } parts64;
} ieee854_long_double_shape_type;

static inline int
__issignalingl (long double x)
{
  uint64_t hxi, lxi;
  ieee854_long_double_shape_type u;

  u.value = x;
  hxi = u.parts64.msw;
  lxi = u.parts64.lsw;

  hxi ^= UINT64_C (0x0000800000000000);
  hxi |= (lxi | -lxi) >> 63;
  return (hxi & UINT64_C (0x7fffffffffffffff)) > UINT64_C (0x7fff800000000000);
}

#else

static inline int
__issignalingl (long double x)
{
  return 0;
}

#endif


#if defined(GFC_REAL_16_IS_FLOAT128)

/* We have a _Float128 type.  */

typedef union
{
  _Float128 value;
  struct
  {
#if __FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__
    uint64_t msw;
    uint64_t lsw;
#elif __FLOAT_WORD_ORDER__ == __ORDER_LITTLE_ENDIAN__
    uint64_t lsw;
    uint64_t msw;
#endif
  } parts64;
} ieee854_float128_shape_type;

static inline int
__issignalingf128 (_Float128 x)
{
  uint64_t hxi, lxi;
  ieee854_float128_shape_type u;

  u.value = x;
  hxi = u.parts64.msw;
  lxi = u.parts64.lsw;

  hxi ^= UINT64_C (0x0000800000000000);
  hxi |= (lxi | -lxi) >> 63;
  return (hxi & UINT64_C (0x7fffffffffffffff)) > UINT64_C (0x7fff800000000000);
}

#endif


/* Define the type-generic macro based on the functions above.  */

#if defined(GFC_REAL_16_IS_FLOAT128)
# define issignaling(X) \
  _Generic ((X), \
	    _Float128: __issignalingf128, \
	    float: __issignalingf, \
	    double: __issignaling, \
	    long double: __issignalingl)(X)
#else
# define issignaling(X) \
  _Generic ((X), \
	    float: __issignalingf, \
	    double: __issignaling, \
	    long double: __issignalingl)(X)
#endif

