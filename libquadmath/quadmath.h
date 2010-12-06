/* GCC Quad-Precision Math Library
   Copyright (C) 2010 Free Software Foundation, Inc.
   Written by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef QUADMATH_H
#define QUADMATH_H

#include <stdlib.h>

// Define the complex type corresponding to __float128
// ("_Complex __float128" is not allowed)
typedef _Complex float __attribute__((mode(TC))) __complex128;


// Prototypes for real functions
extern __float128 acosq (__float128);
extern __float128 acoshq (__float128);
extern __float128 asinq (__float128);
extern __float128 asinhq (__float128);
extern __float128 atanq (__float128);
extern __float128 atanhq (__float128);
extern __float128 atan2q (__float128, __float128);
extern __float128 cbrtq (const __float128);
extern __float128 ceilq (__float128);
extern __float128 copysignq (__float128, __float128);
extern __float128 coshq (__float128);
extern __float128 cosq (__float128);
extern __float128 erfq (__float128);
extern __float128 erfcq (__float128);
extern __float128 expq (__float128);
extern __float128 expm1q (__float128);
extern __float128 fabsq (__float128);
extern int finiteq (const __float128);
extern __float128 floorq (__float128);
extern __float128 fmodq (__float128, __float128);
extern __float128 frexpq (__float128, int *);
extern __float128 hypotq (__float128, __float128);
extern int isinfq (__float128);
extern int isnanq (const __float128);
extern __float128 j0q (__float128);
extern __float128 j1q (__float128);
extern __float128 jnq (int, __float128);
extern __float128 ldexpq (__float128, int);
extern __float128 lgammaq (__float128);
extern long long int llroundq (__float128);
extern __float128 logq (__float128);
extern __float128 log10q (__float128);
extern __float128 log1pq (__float128);
extern long int lroundq (__float128);
extern __float128 modfq (__float128, __float128 *);
extern __float128 nanq (const char *);
extern __float128 nextafterq (__float128, __float128);
extern __float128 powq (__float128, __float128);
extern __float128 remainderq (__float128, __float128);
extern __float128 rintq (__float128);
extern __float128 roundq (__float128);
extern __float128 scalblnq (__float128, long int);
extern __float128 scalbnq (__float128, int);
extern int signbitq (const __float128);
extern void sincosq (__float128, __float128 *, __float128 *);
extern __float128 sinhq (__float128);
extern __float128 sinq (__float128);
extern __float128 sqrtq (const __float128);
extern __float128 tanq (__float128);
extern __float128 tanhq (__float128);
extern __float128 tgammaq (__float128);
extern __float128 truncq (__float128);
extern __float128 y0q (__float128);
extern __float128 y1q (__float128);
extern __float128 ynq (int, __float128);


// Prototypes for complex functions
extern __float128 cabsq (__complex128);
extern __float128 cargq (__complex128);
extern __complex128 ccosq (__complex128);
extern __complex128 ccoshq (__complex128);
extern __complex128 cexpq (__complex128);
extern __complex128 cexpiq (__float128);
extern __complex128 clogq (__complex128);
extern __complex128 clog10q (__complex128);
extern __complex128 cpowq (__complex128, __complex128);
extern __complex128 csinq (__complex128);
extern __complex128 csinhq (__complex128);
extern __complex128 csqrtq (__complex128);
extern __complex128 ctanq (__complex128);
extern __complex128 ctanhq (__complex128);


// Prototypes for our I/O functions
extern int quadmath_strtopQ (const char *, char **, void *);
extern void quadmath_dtoaq (char *, size_t, size_t, __float128);


// Macros
#define FLT128_MAX 1.18973149535723176508575932662800702e4932Q
#define FLT128_MIN 3.36210314311209350626267781732175260e-4932Q
#define FLT128_EPSILON 1.92592994438723585305597794258492732e-34Q
#define FLT128_DENORM_MIN 6.475175119438025110924438958227646552e-4966Q
#define FLT128_MANT_DIG 113
#define FLT128_MIN_EXP (-16381)
#define FLT128_MAX_EXP 16384
// TODO -- One day, we need to add the following macros:
// FLT128_DIG, FLT128_MIN_10_EXP, FLT128_MAX_10_EXP


#define HUGE_VALQ __builtin_huge_valq()
/* The following alternative is valid, but brings the warning:
   (floating constant exceeds range of ‘__float128’)  */
/* #define HUGE_VALQ (__extension__ 0x1.0p32767Q) */

#endif
