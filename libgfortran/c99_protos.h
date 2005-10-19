/* Declarations of various C99 functions 
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfortran; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifndef C99_PROTOS_H
#define C99_PROTOS_H 1

/* float variants of libm functions */
#ifndef HAVE_ACOSF
#define HAVE_ACOSF 1
extern float acosf(float);
#endif

#ifndef HAVE_ACOSHF
#define HAVE_ACOSHF 1
extern float acoshf(float);
#endif

#ifndef HAVE_ASINF
#define HAVE_ASINF 1
extern float asinf(float);
#endif

#ifndef HAVE_ASINHF
#define HAVE_ASINHF 1
extern float asinhf(float);
#endif

#ifndef HAVE_ATAN2F
#define HAVE_ATAN2F 1
extern float atan2f(float, float);
#endif

#ifndef HAVE_ATANF
#define HAVE_ATANF 1
extern float atanf(float);
#endif

#ifndef HAVE_ATANHF
#define HAVE_ATANHF 1
extern float atanhf(float);
#endif

#ifndef HAVE_CEILF
#define HAVE_CEILF 1
extern float ceilf(float);
#endif

#ifndef HAVE_COPYSIGNF
#define HAVE_COPYSIGNF 1
extern float copysignf(float, float);
#endif

#ifndef HAVE_COSF
#define HAVE_COSF 1
extern float cosf(float);
#endif

#ifndef HAVE_COSHF
#define HAVE_COSHF 1
extern float coshf(float);
#endif

#ifndef HAVE_EXPF
#define HAVE_EXPF 1
extern float expf(float);
#endif

#ifndef HAVE_FABSF
#define HAVE_FABSF 1
extern float fabsf(float);
#endif

#ifndef HAVE_FLOORF
#define HAVE_FLOORF 1
extern float floorf(float);
#endif

#ifndef HAVE_FREXPF
#define HAVE_FREXPF 1
extern float frexpf(float, int *);
#endif

#ifndef HAVE_HYPOTF
#define HAVE_HYPOTF 1
extern float hypotf(float, float);
#endif

#ifndef HAVE_LOGF
#define HAVE_LOGF 1
extern float logf(float);
#endif

#ifndef HAVE_LOG10F
#define HAVE_LOG10F 1
extern float log10f(float);
#endif

#ifndef HAVE_SCALBN
#define HAVE_SCALBN 1
extern double scalbn(double, int);
#endif

#ifndef HAVE_SCALBNF
#define HAVE_SCALBNF 1
extern float scalbnf(float, int);
#endif

#ifndef HAVE_SINF
#define HAVE_SINF 1
extern float sinf(float);
#endif

#ifndef HAVE_SINHF
#define HAVE_SINHF 1
extern float sinhf(float);
#endif

#ifndef HAVE_SQRTF
#define HAVE_SQRTF 1
extern float sqrtf(float);
#endif

#ifndef HAVE_TANF
#define HAVE_TANF 1
extern float tanf(float);
#endif

#ifndef HAVE_TANHF
#define HAVE_TANHF 1
extern float tanhf(float);
#endif

#ifndef HAVE_TRUNC
#define HAVE_TRUNC 1
extern double trunc(double);
#endif

#ifndef HAVE_TRUNCF
#define HAVE_TRUNCF 1
extern float truncf(float);
#endif

#ifndef HAVE_NEXTAFTERF
#define HAVE_NEXTAFTERF 1
extern float nextafterf(float, float);
#endif

#ifndef HAVE_POWF
#define HAVE_POWF 1
extern float powf(float, float);
#endif

#ifndef HAVE_ROUND
#define HAVE_ROUND 1
extern double round(double);
#endif

#ifndef HAVE_ROUNDF
#define HAVE_ROUNDF 1
extern float roundf(float);
#endif


/* log10l is needed on all platforms for decimal I/O */
#ifndef HAVE_LOG10L
#define HAVE_LOG10L 1
extern long double log10l(long double);
#endif


/* complex math functions */

#if !defined(HAVE_CABSF)
#define HAVE_CABSF 1
extern float cabsf (float complex);
#endif

#if !defined(HAVE_CABS)
#define HAVE_CABS 1
extern double cabs (double complex);
#endif

#if !defined(HAVE_CABSL) && defined(HAVE_HYPOTL)
#define HAVE_CABSL 1
extern long double cabsl (long double complex);
#endif


#if !defined(HAVE_CARGF)
#define HAVE_CARGF 1
extern float cargf (float complex);
#endif

#if !defined(HAVE_CARG)
#define HAVE_CARG 1
extern double carg (double complex);
#endif

#if !defined(HAVE_CARGL) && defined(HAVE_ATAN2L)
#define HAVE_CARGL 1
extern long double cargl (long double complex);
#endif


#if !defined(HAVE_CEXPF)
#define HAVE_CEXPF 1
extern float complex cexpf (float complex);
#endif

#if !defined(HAVE_CEXP)
#define HAVE_CEXP 1
extern double complex cexp (double complex);
#endif

#if !defined(HAVE_CEXPL) && defined(HAVE_COSL) && defined(HAVE_SINL) && defined(EXPL)
#define HAVE_CEXPL 1
extern long double complex cexpl (long double complex);
#endif


#if !defined(HAVE_CLOGF)
#define HAVE_CLOGF 1
extern float complex clogf (float complex);
#endif

#if !defined(HAVE_CLOG)
#define HAVE_CLOG 1
extern double complex clog (double complex);
#endif

#if !defined(HAVE_CLOGL) && defined(HAVE_LOGL) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOGL 1
extern long double complex clogl (long double complex);
#endif


#if !defined(HAVE_CLOG10F)
#define HAVE_CLOG10F 1
extern float complex clog10f (float complex);
#endif

#if !defined(HAVE_CLOG10)
#define HAVE_CLOG10 1
extern double complex clog10 (double complex);
#endif

#if !defined(HAVE_CLOG10L) && defined(HAVE_LOG10L) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOG10L 1
extern long double complex clog10l (long double complex);
#endif


#if !defined(HAVE_CPOWF)
#define HAVE_CPOWF 1
extern float complex cpowf (float complex, float complex);
#endif

#if !defined(HAVE_CPOW)
#define HAVE_CPOW 1
extern double complex cpow (double complex, double complex);
#endif

#if !defined(HAVE_CPOWL) && defined(HAVE_CEXPL) && defined(HAVE_CLOGL)
#define HAVE_CPOWL 1
extern long double complex cpowl (long double complex, long double complex);
#endif


#if !defined(HAVE_CSQRTF)
#define HAVE_CSQRTF 1 
extern float complex csqrtf (float complex);
#endif

#if !defined(HAVE_CSQRT)
#define HAVE_CSQRT 1
extern double complex csqrt (double complex);
#endif

#if !defined(HAVE_CSQRTL) && defined(HAVE_COPYSIGNL) && defined(HAVE_SQRTL) && defined(HAVE_FABSL) && defined(HAVE_HYPOTL)
#define HAVE_CSQRTL 1
extern long double complex csqrtl (long double complex);
#endif


#if !defined(HAVE_CSINHF)
#define HAVE_CSINHF 1
extern float complex csinhf (float complex);
#endif

#if !defined(HAVE_CSINH)
#define HAVE_CSINH 1
extern double complex csinh (double complex);
#endif

#if !defined(HAVE_CSINHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINHL 1
extern long double complex csinhl (long double complex);
#endif


#if !defined(HAVE_CCOSHF)
#define HAVE_CCOSHF 1
extern float complex ccoshf (float complex);
#endif

#if !defined(HAVE_CCOSH)
#define HAVE_CCOSH 1
extern double complex ccosh (double complex);
#endif

#if !defined(HAVE_CCOSHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSHL 1
extern long double complex ccoshl (long double complex);
#endif


#if !defined(HAVE_CTANHF)
#define HAVE_CTANHF 1
extern float complex ctanhf (float complex);
#endif

#if !defined(HAVE_CTANH)
#define HAVE_CTANH 1
extern double complex ctanh (double complex);
#endif

#if !defined(HAVE_CTANHL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANHL 1
extern long double complex ctanhl (long double complex);
#endif


#if !defined(HAVE_CSINF)
#define HAVE_CSINF 1
extern float complex csinf (float complex);
#endif

#if !defined(HAVE_CSIN)
#define HAVE_CSIN 1
extern double complex csin (double complex);
#endif

#if !defined(HAVE_CSINL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINL 1
extern long double complex csinl (long double complex);
#endif


#if !defined(HAVE_CCOSF)
#define HAVE_CCOSF 1
extern float complex ccosf (float complex);
#endif

#if !defined(HAVE_CCOS)
#define HAVE_CCOS 1
extern double complex ccos (double complex);
#endif

#if !defined(HAVE_CCOSL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSL 1
extern long double complex ccosl (long double complex);
#endif


#if !defined(HAVE_CTANF)
#define HAVE_CTANF 1
extern float complex ctanf (float complex);
#endif

#if !defined(HAVE_CTAN)
#define HAVE_CTAN 1
extern double complex ctan (double complex);
#endif

#if !defined(HAVE_CTANL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANL 1
extern long double complex ctanl (long double complex);
#endif


#endif  /* C99_PROTOS_H  */

