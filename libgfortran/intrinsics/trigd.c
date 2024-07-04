/* Implementation of the degree trignometric functions COSD, SIND, TAND.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargl@gcc.gnu.org>

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

#include <math.h>

/* Body of library functions which are cannot be implemented on the current
 * platform because it lacks a capability, such as an underlying trigonometric
 * function (sin, cos, tan) or C99 floating-point function (fabs, fmod). */
#define STRINGIFY_EXPAND(x) #x
#define ERROR_RETURN(f, k, x) runtime_error (#f " is unavailable for" \
    " REAL(KIND=" STRINGIFY_EXPAND(k) ") because the system math library" \
    " lacks support for it"); \
    RETURN(x)

/*
   For real x, let {x}_P or x_P be the closest representible number in the
   floating point representation which uses P binary bits of fractional
   precision (with IEEE rounding semantics).

   Similarly, let f_P(x) be shorthand for {f(x)}_P.

   Let ulp_P(x) be the unit of least precision for x: in other words the
   maximal value of |a_P - b_P| where a_P <= x <= b_P and a_P != b_P.

   Let x  ~= y <-> | x - y | <  ulp_P(x - y).

   Let deg(x) be the value of x radians in degrees.

   Values for each precision P were selected as follows.


   COSD_SMALL = 2**{-N} such that for all x <= COSD_SMALL:

     * cos(deg(x)) ~= 1, or equivalently:

       |      1 - cos(deg(x))  | < ulp_P(1).

   Unfortunately for SIND (and therefore TAND) a similar relation is only
   possible for REAL(4) and REAL(8). With REAL(10) and REAL(16), enough
   precision is available such that sin_P(x) != x_P for some x less than any
   value. (There are values where this equality holds, but the distance has
   inflection points.)

   For REAL(4) and REAL(8), we can select SIND_SMALL such that:

     * sin(deg(x)) ~= deg(x), or equivalently:

       | deg(x) - sin(deg(x)) | < ulp_P(deg(x)).

 */

#ifdef HAVE_GFC_REAL_4

/* Build _gfortran_sind_r4, _gfortran_cosd_r4, and _gfortran_tand_r4  */

#define KIND	4
#define TINY	0x1.p-100	/* ~= 7.889e-31 */
#define COSD_SMALL  0x1.p-7	/*  = 7.8125e-3 */
#define SIND_SMALL  0x1.p-5	/*  = 3.125e-2 */
#define COSD30      8.66025388e-01
#define PIO180H     1.74560547e-02	/* high 12 bits.  */
#define PIO180L    -2.76216747e-06	/* Next 24 bits.  */

#if defined(HAVE_FABSF) && defined(HAVE_FMODF) && defined(HAVE_COPYSIGNF)

#ifdef HAVE_SINF
#define ENABLE_SIND
#endif

#ifdef HAVE_COSF
#define ENABLE_COSD
#endif

#ifdef HAVE_TANF
#define ENABLE_TAND
#endif

#endif /* HAVE_FABSF && HAVE_FMODF && HAVE_COPYSIGNF */

#ifdef GFC_REAL_4_INFINITY
#define HAVE_INFINITY_KIND
#endif

#include "trigd_lib.inc"

#undef KIND
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef ENABLE_SIND
#undef ENABLE_COSD
#undef ENABLE_TAND
#undef HAVE_INFINITY_KIND

#endif /* HAVE_GFC_REAL_4... */


#ifdef HAVE_GFC_REAL_8

/* Build _gfortran_sind_r8, _gfortran_cosd_r8, and _gfortran_tand_r8  */

#define KIND	8
#define TINY	0x1.p-1000	/* ~= 9.33e-302 (min exp -1074) */
#define COSD_SMALL  0x1.p-21	/* ~= 4.768e-7 */
#define SIND_SMALL  0x1.p-19	/* ~= 9.537e-7 */
#define COSD30      8.6602540378443860e-01
#define PIO180H     1.7453283071517944e-02	/* high 21 bits.  */
#define PIO180L     9.4484253514332993e-09	/* Next 53 bits.  */

#if defined(HAVE_FABS) && defined(HAVE_FMOD) && defined(HAVE_COPYSIGN)

#ifdef HAVE_SIN
#define ENABLE_SIND
#endif

#ifdef HAVE_COS
#define ENABLE_COSD
#endif

#ifdef HAVE_TAN
#define ENABLE_TAND
#endif

#endif /* HAVE_FABS && HAVE_FMOD && HAVE_COPYSIGN */

#ifdef GFC_REAL_8_INFINITY
#define HAVE_INFINITY_KIND
#endif

#include "trigd_lib.inc"

#undef KIND
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef ENABLE_SIND
#undef ENABLE_COSD
#undef ENABLE_TAND
#undef HAVE_INFINITY_KIND

#endif /* HAVE_GFC_REAL_8... */


#ifdef HAVE_GFC_REAL_10

/* Build _gfortran_sind_r10, _gfortran_cosd_r10, and _gfortran_tand_r10  */

#define KIND	10
#define TINY	0x1.p-16400	/* ~= 1.28e-4937 (min exp -16494) */
#define COSD_SMALL  0x1.p-26	/* ~= 1.490e-8 */
#undef  SIND_SMALL		/* not precise */
#define COSD30      8.66025403784438646787e-01
#define PIO180H     1.74532925229868851602e-02	/* high 32 bits */
#define PIO180L    -3.04358939097084072823e-12	/* Next 64 bits */

#if defined(HAVE_FABSL) && defined(HAVE_FMODL) && defined(HAVE_COPYSIGNL)

#ifdef HAVE_SINL
#define ENABLE_SIND
#endif

#ifdef HAVE_COSL
#define ENABLE_COSD
#endif

#ifdef HAVE_TANL
#define ENABLE_TAND
#endif

#endif /* HAVE_FABSL && HAVE_FMODL && HAVE_COPYSIGNL */

#ifdef GFC_REAL_10_INFINITY
#define HAVE_INFINITY_KIND
#endif

#include "trigd_lib.inc"

#undef KIND
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef ENABLE_SIND
#undef ENABLE_COSD
#undef ENABLE_TAND
#undef HAVE_INFINITY_KIND

#endif /* HAVE_GFC_REAL_10 */


#ifdef HAVE_GFC_REAL_16

/* Build _gfortran_sind_r16, _gfortran_cosd_r16, and _gfortran_tand_r16  */

#define KIND	16
#define TINY	0x1.p-16400	/* ~= 1.28e-4937 */
#undef  SIND_SMALL		/* not precise */

#if GFC_REAL_16_DIGITS == 64
/* 80 bit precision, use constants from REAL(10).  */
#define COSD_SMALL  0x1.p-26	/* ~= 1.490e-8 */
#define COSD30      8.66025403784438646787e-01
#define PIO180H     1.74532925229868851602e-02	/* high 32 bits */
#define PIO180L    -3.04358939097084072823e-12	/* Next 64 bits */

#else
/* Proper float128 precision.  */
#define COSD_SMALL  0x1.p-51	/* ~= 4.441e-16 */
#define COSD30      8.66025403784438646763723170752936183e-01
#define PIO180H     1.74532925199433197605003442731685936e-02
#define PIO180L     -2.39912634365882824665106671063098954e-17
#endif

#ifdef GFC_REAL_16_IS_LONG_DOUBLE

#if defined(HAVE_FABSL) && defined(HAVE_FMODL) && defined(HAVE_COPYSIGNL)

#ifdef HAVE_SINL
#define ENABLE_SIND
#endif

#ifdef HAVE_COSL
#define ENABLE_COSD
#endif

#ifdef HAVE_TANL
#define ENABLE_TAND
#endif

#endif /* HAVE_FABSL && HAVE_FMODL && HAVE_COPYSIGNL */

#elif defined(GFC_REAL_16_USE_IEC_60559)

#if defined(HAVE_FABSF128) && defined(HAVE_FMODF128) && defined(HAVE_COPYSIGNF128)

#ifdef HAVE_SINF128
#define ENABLE_SIND
#endif

#ifdef HAVE_COSF128
#define ENABLE_COSD
#endif

#ifdef HAVE_TANF128
#define ENABLE_TAND
#endif

#endif /* HAVE_FABSF128 && HAVE_FMODF128 && HAVE_COPYSIGNF128 */

#else

/* libquadmath: HAVE_*Q are never defined.  They must be available.  */
#define ENABLE_SIND
#define ENABLE_COSD
#define ENABLE_TAND

#endif /* GFC_REAL_16_IS_LONG_DOUBLE */

#ifdef GFC_REAL_16_INFINITY
#define HAVE_INFINITY_KIND
#endif

#include "trigd_lib.inc"

#undef KIND
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef ENABLE_SIND
#undef ENABLE_COSD
#undef ENABLE_TAND
#undef HAVE_INFINITY_KIND

#endif /* HAVE_GFC_REAL_16 */

#ifdef HAVE_GFC_REAL_17

/* Build _gfortran_sind_r17, _gfortran_cosd_r17, and _gfortran_tand_r17  */

#define KIND	17
#define TINY	0x1.p-16400	/* ~= 1.28e-4937 */
#undef  SIND_SMALL		/* not precise */

/* Proper float128 precision.  */
#define COSD_SMALL  0x1.p-51	/* ~= 4.441e-16 */
#define COSD30      8.66025403784438646763723170752936183e-01
#define PIO180H     1.74532925199433197605003442731685936e-02
#define PIO180L     -2.39912634365882824665106671063098954e-17

/* libquadmath or glibc 2.32+: HAVE_*Q are never defined.  They must be available.  */
#define ENABLE_SIND
#define ENABLE_COSD
#define ENABLE_TAND

#ifdef GFC_REAL_17_INFINITY
#define HAVE_INFINITY_KIND
#endif

#ifdef POWER_IEEE128
#define COPYSIGN __copysignieee128
#define FMOD __fmodieee128
#define FABS __fabsieee128
#define FMA __fmaieee128
#define SIN __sinieee128
#define COS __cosieee128
#define TAN __tanieee128
#endif

#include "trigd_lib.inc"

#undef KIND
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef ENABLE_SIND
#undef ENABLE_COSD
#undef ENABLE_TAND
#undef HAVE_INFINITY_KIND

#endif /* HAVE_GFC_REAL_17 */
