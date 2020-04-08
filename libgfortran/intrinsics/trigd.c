/* Implementation of the degree trignometric functions COSD, SIND, TAND.
   Copyright (C) 2020 Free Software Foundation, Inc.
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

/* Build _gfortran_sind_r4, _gfortran_cosd_r4, and _gfortran_tand_r4  */

#define FTYPE       GFC_REAL_4
#define SIND        sind_r4
#define COSD        cosd_r4
#define TAND        tand_r4
#define SUFFIX(x)   x ## f

#define TINY        0x1.p-100f	/* ~= 7.889e-31 */
#define COSD_SMALL  0x1.p-7f	/*  = 7.8125e-3 */
#define SIND_SMALL  0x1.p-5f	/*  = 3.125e-2 */
#define COSD30      8.66025388e-01f

#define PIO180H     1.74560547e-02f	/* high 12 bits.  */
#define PIO180L    -2.76216747e-06f	/* Next 24 bits.  */

#include "trigd_lib.inc"

#undef FTYPE
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef SIND
#undef COSD
#undef TAND
#undef SUFFIX


/* Build _gfortran_sind_r8, _gfortran_cosd_r8, and _gfortran_tand_r8.  */

#define FTYPE       GFC_REAL_8
#define SIND        sind_r8
#define COSD        cosd_r8
#define TAND        tand_r8
#define SUFFIX(x)   x

#define TINY        0x1.p-1000	/* ~= 9.33e-302 (min exp -1074) */
#define COSD_SMALL  0x1.p-21	/* ~= 4.768e-7 */
#define SIND_SMALL  0x1.p-19	/* ~= 9.537e-7 */
#define COSD30      8.6602540378443860e-01

#define PIO180H     1.7453283071517944e-02	/* high 21 bits.  */
#define PIO180L     9.4484253514332993e-09	/* Next 53 bits.  */

#include "trigd_lib.inc"

#undef FTYPE
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef SIND
#undef COSD
#undef TAND
#undef SUFFIX


/* Build _gfortran_sind_r10, _gfortran_cosd_r10, and _gfortran_tand_r10.  */

#ifdef HAVE_GFC_REAL_10

#define FTYPE       GFC_REAL_10
#define SIND        sind_r10
#define COSD        cosd_r10
#define TAND        tand_r10
#define SUFFIX(x)   x ## l	/* L */

#define TINY        0x1.p-16400L	/* ~= 1.28e-4937 (min exp -16494) */
#define COSD_SMALL  0x1.p-26L	/* ~= 1.490e-8 */
#undef  SIND_SMALL		/* not precise */
#define COSD30       8.66025403784438646787e-01L

#define PIO180H     1.74532925229868851602e-02L	/* high 32 bits */
#define PIO180L    -3.04358939097084072823e-12L	/* Next 64 bits */

#include "trigd_lib.inc"
#undef FTYPE
#undef TINY
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef SIND
#undef COSD
#undef TAND
#undef SUFFIX
#endif /* HAVE_GFC_REAL_10 */


/* Build _gfortran_sind_r16, _gfortran_cosd_r16, and _gfortran_tand_r16.  */

#ifdef HAVE_GFC_REAL_16

#define FTYPE       GFC_REAL_16
#define SIND        sind_r16
#define COSD        cosd_r16
#define TAND        tand_r16

#ifdef GFC_REAL_16_IS_FLOAT128	/* libquadmath.  */
#define SUFFIX(x) x ## q
#else
#define SUFFIX(x) x ## l
#endif /* GFC_REAL_16_IS_FLOAT128  */

#define TINY        SUFFIX(0x1.p-16400)	/* ~= 1.28e-4937 */
#define COSD_SMALL  SUFFIX(0x1.p-51)	/* ~= 4.441e-16 */
#undef  SIND_SMALL		/* not precise */
#define COSD30      SUFFIX(8.66025403784438646763723170752936183e-01)
#define PIO180H     SUFFIX(1.74532925199433197605003442731685936e-02)
#define PIO180L     SUFFIX(-2.39912634365882824665106671063098954e-17)

#include "trigd_lib.inc"

#undef FTYPE
#undef COSD_SMALL
#undef SIND_SMALL
#undef COSD30
#undef PIO180H
#undef PIO180L
#undef PIO180
#undef D2R
#undef CPYSGN
#undef FABS
#undef FMOD
#undef SIN
#undef COS
#undef TAN
#undef SIND
#undef COSD
#undef TAND
#undef SUFFIX
#endif /* HAVE_GFC_REAL_16 */
