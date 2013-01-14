/* Declarations of various C99 functions 
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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

#ifndef C99_PROTOS_H
#define C99_PROTOS_H 1

/* float variants of libm functions */
#ifndef HAVE_ACOSF
#define HAVE_ACOSF 1
extern float acosf(float);
#endif

#if HAVE_ACOSH && !HAVE_ACOSHF
#define HAVE_ACOSHF 1
extern float acoshf(float);
#endif

#ifndef HAVE_ASINF
#define HAVE_ASINF 1
extern float asinf(float);
#endif

#if HAVE_ASINH && !HAVE_ASINHF
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

#if HAVE_ATANH && !HAVE_ATANHF
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

#ifndef HAVE_FLOORL
#define HAVE_FLOORL 1
extern long double floorl (long double x);
#endif

#ifndef HAVE_FMODF
#define HAVE_FMODF 1
extern float fmodf (float x, float y);
#endif

#ifndef HAVE_FMODL
#define HAVE_FMODL 1
extern long double fmodl (long double x, long double y);
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

#if !defined(HAVE_ROUNDL)
#define HAVE_ROUNDL 1
extern long double roundl(long double);
#endif



#if !defined(HAVE_LROUNDF) && defined(HAVE_ROUNDF)
#define HAVE_LROUNDF 1
long int lroundf (float);
#endif

#if !defined(HAVE_LROUND) && defined(HAVE_ROUND)
#define HAVE_LROUND 1
long int lround (double);
#endif

#if !defined(HAVE_LROUNDL) && defined(HAVE_ROUNDL)
#define HAVE_LROUNDL 1
long int lroundl (long double);
#endif

#if !defined(HAVE_LLROUNDF) && defined(HAVE_ROUNDF)
#define HAVE_LLROUNDF 1
long long int llroundf (float);
#endif

#if !defined(HAVE_LLROUND) && defined(HAVE_ROUND)
#define HAVE_LLROUND 1
long long int llround (double);
#endif

#if !defined(HAVE_LLROUNDL) && defined(HAVE_ROUNDL)
#define HAVE_LLROUNDL 1
long long int llroundl (long double);
#endif

/* Wrappers for systems without the various C99 single precision Bessel
   functions.  */

#if defined(HAVE_J0) && ! defined(HAVE_J0F)
#define HAVE_J0F 1
extern float j0f (float);
#endif

#if defined(HAVE_J1) && !defined(HAVE_J1F)
#define HAVE_J1F 1
extern float j1f (float);
#endif

#if defined(HAVE_JN) && !defined(HAVE_JNF)
#define HAVE_JNF 1
extern float jnf (int, float);
#endif

#if defined(HAVE_Y0) && !defined(HAVE_Y0F)
#define HAVE_Y0F 1
extern float y0f (float);
#endif

#if defined(HAVE_Y1) && !defined(HAVE_Y1F)
#define HAVE_Y1F 1
extern float y1f (float);
#endif

#if defined(HAVE_YN) && !defined(HAVE_YNF)
#define HAVE_YNF 1
extern float ynf (int, float);
#endif


/* Wrappers for systems without the C99 erff() and erfcf() functions.  */

#if defined(HAVE_ERF) && !defined(HAVE_ERFF)
#define HAVE_ERFF 1
extern float erff (float);
#endif

#if defined(HAVE_ERFC) && !defined(HAVE_ERFCF)
#define HAVE_ERFCF 1
extern float erfcf (float);
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


/* Complex ACOS.  */

#if !defined(HAVE_CACOSF) && defined(HAVE_CLOGF) && defined(HAVE_CSQRTF)
#define HAVE_CACOSF 1
extern complex float cacosf (complex float z);
#endif

#if !defined(HAVE_CACOS) && defined(HAVE_CLOG) && defined(HAVE_CSQRT)
#define HAVE_CACOS 1
extern complex double cacos (complex double z);
#endif

#if !defined(HAVE_CACOSL) && defined(HAVE_CLOGL) && defined(HAVE_CSQRTL)
#define HAVE_CACOSL 1
extern complex long double cacosl (complex long double z);
#endif


/* Complex ASIN.  */

#if !defined(HAVE_CASINF) && defined(HAVE_CLOGF) && defined(HAVE_CSQRTF)
#define HAVE_CASINF 1
extern complex float casinf (complex float z);
#endif

#if !defined(HAVE_CASIN) && defined(HAVE_CLOG) && defined(HAVE_CSQRT)
#define HAVE_CASIN 1
extern complex double casin (complex double z);
#endif

#if !defined(HAVE_CASINL) && defined(HAVE_CLOGL) && defined(HAVE_CSQRTL)
#define HAVE_CASINL 1
extern complex long double casinl (complex long double z);
#endif


/* Complex ATAN.  */

#if !defined(HAVE_CATANF) && defined(HAVE_CLOGF)
#define HAVE_CATANF 1
extern complex float catanf (complex float z);
#endif

#if !defined(HAVE_CATAN) && defined(HAVE_CLOG)
#define HAVE_CATAN 1
extern complex double catan (complex double z);
#endif

#if !defined(HAVE_CATANL) && defined(HAVE_CLOGL)
#define HAVE_CATANL 1
extern complex long double catanl (complex long double z);
#endif


/* Complex ASINH.  */

#if !defined(HAVE_CASINHF) && defined(HAVE_CLOGF) && defined(HAVE_CSQRTF)
#define HAVE_CASINHF 1
extern complex float casinhf (complex float z);
#endif


#if !defined(HAVE_CASINH) && defined(HAVE_CLOG) && defined(HAVE_CSQRT)
#define HAVE_CASINH 1
extern complex double casinh (complex double z);
#endif

#if !defined(HAVE_CASINHL) && defined(HAVE_CLOGL) && defined(HAVE_CSQRTL)
#define HAVE_CASINHL 1
extern complex long double casinhl (complex long double z);
#endif


/* Complex ACOSH.  */

#if !defined(HAVE_CACOSHF) && defined(HAVE_CLOGF) && defined(HAVE_CSQRTF)
#define HAVE_CACOSHF 1
extern complex float cacoshf (complex float z);
#endif

#if !defined(HAVE_CACOSH) && defined(HAVE_CLOG) && defined(HAVE_CSQRT)
#define HAVE_CACOSH 1
extern complex double cacosh (complex double z);
#endif

#if !defined(HAVE_CACOSHL) && defined(HAVE_CLOGL) && defined(HAVE_CSQRTL)
#define HAVE_CACOSHL 1
extern complex long double cacoshl (complex long double z);
#endif


/* Complex ATANH.  */

#if !defined(HAVE_CATANHF) && defined(HAVE_CLOGF)
#define HAVE_CATANHF 1
extern complex float catanhf (complex float z);
#endif

#if !defined(HAVE_CATANH) && defined(HAVE_CLOG)
#define HAVE_CATANH 1
extern complex double catanh (complex double z);
#endif

#if !defined(HAVE_CATANHL) && defined(HAVE_CLOGL)
#define HAVE_CATANHL 1
extern complex long double catanhl (complex long double z);
#endif


/* Gamma-related prototypes.  */
#if !defined(HAVE_TGAMMA)
#define HAVE_TGAMMA 1
extern double tgamma (double);
#endif

#if !defined(HAVE_LGAMMA)
#define HAVE_LGAMMA 1
extern double lgamma (double);
#endif

#if defined(HAVE_TGAMMA) && !defined(HAVE_TGAMMAF)
#define HAVE_TGAMMAF 1
extern float tgammaf (float);
#endif

#if defined(HAVE_LGAMMA) && !defined(HAVE_LGAMMAF)
#define HAVE_LGAMMAF 1
extern float lgammaf (float);
#endif


#endif  /* C99_PROTOS_H  */

