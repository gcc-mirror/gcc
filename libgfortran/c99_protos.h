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
#define C99_PROTOS_H

/* float variants of libm functions */
#ifndef HAVE_ACOSF
#define HAVE_ACOSF
extern float acosf(float);
#endif

#ifndef HAVE_ACOSHF
#define HAVE_ACOSHF
extern float acoshf(float);
#endif

#ifndef HAVE_ASINF
#define HAVE_ASINF
extern float asinf(float);
#endif

#ifndef HAVE_ASINHF
#define HAVE_ASINHF
extern float asinhf(float);
#endif

#ifndef HAVE_ATAN2F
#define HAVE_ATAN2F
extern float atan2f(float, float);
#endif

#ifndef HAVE_ATANF
#define HAVE_ATANF
extern float atanf(float);
#endif

#ifndef HAVE_ATANHF
#define HAVE_ATANHF
extern float atanhf(float);
#endif

#ifndef HAVE_CEILF
#define HAVE_CEILF
extern float ceilf(float);
#endif

#ifndef HAVE_COPYSIGNF
#define HAVE_COPYSIGNF
extern float copysignf(float, float);
#endif

#ifndef HAVE_COSF
#define HAVE_COSF
extern float cosf(float);
#endif

#ifndef HAVE_COSHF
#define HAVE_COSHF
extern float coshf(float);
#endif

#ifndef HAVE_EXPF
#define HAVE_EXPF
extern float expf(float);
#endif

#ifndef HAVE_FABSF
#define HAVE_FABSF
extern float fabsf(float);
#endif

#ifndef HAVE_FLOORF
#define HAVE_FLOORF
extern float floorf(float);
#endif

#ifndef HAVE_FREXPF
#define HAVE_FREXPF
extern float frexpf(float, int *);
#endif

#ifndef HAVE_HYPOTF
#define HAVE_HYPOTF
extern float hypotf(float, float);
#endif

#ifndef HAVE_LOGF
#define HAVE_LOGF
extern float logf(float);
#endif

#ifndef HAVE_LOG10F
#define HAVE_LOG10F
extern float log10f(float);
#endif

#ifndef HAVE_SCALBN
#define HAVE_SCALBN
extern double scalbn(double, int);
#endif

#ifndef HAVE_SCALBNF
#define HAVE_SCALBNF
extern float scalbnf(float, int);
#endif

#ifndef HAVE_SINF
#define HAVE_SINF
extern float sinf(float);
#endif

#ifndef HAVE_SINHF
#define HAVE_SINHF
extern float sinhf(float);
#endif

#ifndef HAVE_SQRTF
#define HAVE_SQRTF
extern float sqrtf(float);
#endif

#ifndef HAVE_TANF
#define HAVE_TANF
extern float tanf(float);
#endif

#ifndef HAVE_TANHF
#define HAVE_TANHF
extern float tanhf(float);
#endif

#ifndef HAVE_TRUNC
#define HAVE_TRUNC
extern double trunc(double);
#endif

#ifndef HAVE_TRUNCF
#define HAVE_TRUNCF
extern float truncf(float);
#endif

#ifndef HAVE_NEXTAFTERF
#define HAVE_NEXTAFTERF
extern float nextafterf(float, float);
#endif

#ifndef HAVE_POWF
#define HAVE_POWF
extern float powf(float, float);
#endif

#ifndef HAVE_ROUND
#define HAVE_ROUND
extern double round(double);
#endif

#ifndef HAVE_ROUNDF
#define HAVE_ROUNDF
extern float roundf(float);
#endif


/* log10l is needed on all platforms for decimal I/O */
#ifndef HAVE_LOG10L
#define HAVE_LOG10L
extern long double log10l(long double);
#endif


/* complex math functions */

#if !defined(HAVE_CABSF)
#define HAVE_CABSF
extern float cabsf (float complex);
#endif

#if !defined(HAVE_CABS)
#define HAVE_CABS
extern double cabs (double complex);
#endif

#if !defined(HAVE_CABSL) && defined(HAVE_HYPOTL)
#define HAVE_CABSL
extern long double cabsl (long double complex);
#endif


#if !defined(HAVE_CARGF)
#define HAVE_CARGF
extern float cargf (float complex);
#endif

#if !defined(HAVE_CARG)
#define HAVE_CARG
extern double carg (double complex);
#endif

#if !defined(HAVE_CARGL) && defined(HAVE_ATAN2L)
#define HAVE_CARGL
extern long double cargl (long double complex);
#endif


#if !defined(HAVE_CEXPF)
#define HAVE_CEXPF
extern float complex cexpf (float complex);
#endif

#if !defined(HAVE_CEXP)
#define HAVE_CEXP
extern double complex cexp (double complex);
#endif

#if !defined(HAVE_CEXPL) && defined(HAVE_COSL) && defined(HAVE_SINL) && defined(EXPL)
#define HAVE_CEXPL
extern long double complex cexpl (long double complex);
#endif


#if !defined(HAVE_CLOGF)
#define HAVE_CLOGF
extern float complex clogf (float complex);
#endif

#if !defined(HAVE_CLOG)
#define HAVE_CLOG
extern double complex clog (double complex);
#endif

#if !defined(HAVE_CLOGL) && defined(HAVE_LOGL) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOGL
extern long double complex clogl (long double complex);
#endif


#if !defined(HAVE_CLOG10F)
#define HAVE_CLOG10F
extern float complex clog10f (float complex);
#endif

#if !defined(HAVE_CLOG10)
#define HAVE_CLOG10
extern double complex clog10 (double complex);
#endif

#if !defined(HAVE_CLOG10L) && defined(HAVE_LOG10L) && defined(HAVE_CABSL) && defined(HAVE_CARGL)
#define HAVE_CLOG10L
extern long double complex clog10l (long double complex);
#endif


#if !defined(HAVE_CPOWF)
#define HAVE_CPOWF
extern float complex cpowf (float complex, float complex);
#endif

#if !defined(HAVE_CPOW)
#define HAVE_CPOW
extern double complex cpow (double complex, double complex);
#endif

#if !defined(HAVE_CPOWL) && defined(HAVE_CEXPL) && defined(HAVE_CLOGL)
#define HAVE_CPOWL
extern long double complex cpowl (long double complex, long double complex);
#endif


#if !defined(HAVE_CSQRTF)
#define HAVE_CSQRTF
extern float complex csqrtf (float complex);
#endif

#if !defined(HAVE_CSQRT)
#define HAVE_CSQRT
extern double complex csqrt (double complex);
#endif

#if !defined(HAVE_CSQRTL) && defined(HAVE_COPYSIGNL) && defined(HAVE_SQRTL) && defined(HAVE_FABSL) && defined(HAVE_HYPOTL)
#define HAVE_CSQRTL
extern long double complex csqrtl (long double complex);
#endif


#if !defined(HAVE_CSINHF)
#define HAVE_CSINHF
extern float complex csinhf (float complex);
#endif

#if !defined(HAVE_CSINH)
#define HAVE_CSINH
extern double complex csinh (double complex);
#endif

#if !defined(HAVE_CSINHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINHL
extern long double complex csinhl (long double complex);
#endif


#if !defined(HAVE_CCOSHF)
#define HAVE_CCOSHF
extern float complex ccoshf (float complex);
#endif

#if !defined(HAVE_CCOSH)
#define HAVE_CCOSH
extern double complex ccosh (double complex);
#endif

#if !defined(HAVE_CCOSHL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSHL
extern long double complex ccoshl (long double complex);
#endif


#if !defined(HAVE_CTANHF)
#define HAVE_CTANHF
extern float complex ctanhf (float complex);
#endif

#if !defined(HAVE_CTANH)
#define HAVE_CTANH
extern double complex ctanh (double complex);
#endif

#if !defined(HAVE_CTANHL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANHL
extern long double complex ctanhl (long double complex);
#endif


#if !defined(HAVE_CSINF)
#define HAVE_CSINF
extern float complex csinf (float complex);
#endif

#if !defined(HAVE_CSIN)
#define HAVE_CSIN
extern double complex csin (double complex);
#endif

#if !defined(HAVE_CSINL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CSINL
extern long double complex csinl (long double complex);
#endif


#if !defined(HAVE_CCOSF)
#define HAVE_CCOSF
extern float complex ccosf (float complex);
#endif

#if !defined(HAVE_CCOS)
#define HAVE_CCOS
extern double complex ccos (double complex);
#endif

#if !defined(HAVE_CCOSL) && defined(HAVE_COSL) && defined(HAVE_COSHL) && defined(HAVE_SINL) && defined(HAVE_SINHL)
#define HAVE_CCOSL
extern long double complex ccosl (long double complex);
#endif


#if !defined(HAVE_CTANF)
#define HAVE_CTANF
extern float complex ctanf (float complex);
#endif

#if !defined(HAVE_CTAN)
#define HAVE_CTAN
extern double complex ctan (double complex);
#endif

#if !defined(HAVE_CTANL) && defined(HAVE_TANL) && defined(HAVE_TANHL)
#define HAVE_CTANL
extern long double complex ctanl (long double complex);
#endif


#endif  /* C99_PROTOS_H  */

