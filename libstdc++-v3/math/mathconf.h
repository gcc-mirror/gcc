/* Configuration data for libmath subpart of libstdc++. */

/* Copyright (C) 1997-1999 Free Software Foundation, Inc.

   This file is part of the GNU ISO C++ Library.  This library is free
   software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this library; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.

   As a special exception, you may use this file as part of a free software
   library without restriction.  Specifically, if other files instantiate
   templates or use macros or inline functions from this file, or you compile
   this file and link it with other files to produce an executable, this
   file does not by itself cause the resulting executable to be covered by
   the GNU General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be covered by
   the GNU General Public License.  */


#include <bits/c++config.h>

#ifdef _GLIBCPP_HAVE_ENDIAN_H
# include <endian.h>
#else
# ifdef _GLIBCPP_HAVE_MACHINE_ENDIAN_H
#  include <machine/endian.h>
# else
#  ifdef _GLIBCPP_HAVE_SYS_MACHINE_H
#   include <sys/machine.h>
#  else
#   if defined _GLIBCPP_HAVE_SYS_ISA_DEFS_H || defined _GLIBCPP_HAVE_MACHINE_PARAM_H
/* This is on Solaris.  */
#    ifdef _GLIBCPP_HAVE_SYS_ISA_DEFS_H
#     include <sys/isa_defs.h>
#    endif
#    ifdef _GLIBCPP_HAVE_MACHINE_PARAM_H
#     include <machine/param.h>
#    endif
#    ifdef _LITTLE_ENDIAN
#     define LITTLE_ENDIAN 1
#    endif
#    ifdef _BIG_ENDIAN
#     define BIG_ENDIAN 1
#    endif
#    define BYTE_ORDER 1
#   else
/* We have to rely on the AC_C_BIGENDIAN test.  */
#    ifdef WORDS_BIGENDIAN
#     define BIG_ENDIAN 1
#    else
#     define LITTLE_ENDIAN 1
#    endif
#    define BYTE_ORDER 1
#   endif
#  endif
# endif
#endif

typedef unsigned int U_int32_t __attribute ((mode (SI)));
typedef int Int32_t __attribute ((mode (SI)));

#ifdef _GLIBCPP_HAVE_NAN_H
# include <nan.h>
#endif

#ifdef _GLIBCPP_HAVE_IEEEFP_H
# include <ieeefp.h>
#endif

#ifdef _GLIBCPP_HAVE_FP_H
# include <fp.h>
#endif

#ifdef _GLIBCPP_HAVE_FLOAT_H
# include <float.h>
#endif

/* `float' variant of HUGE_VAL.  */
#ifndef HUGE_VALF
# ifdef HUGE_VALf
#  define HUGE_VALF HUGE_VALf
# else
#  define HUGE_VALF HUGE_VAL
# endif
#endif

/* `long double' variant of HUGE_VAL.  */
#ifndef HUGE_VALL
# ifdef HUGE_VALl
#  define HUGE_VALL HUGE_VALl
# else
#  define HUGE_VALL HUGE_VAL
# endif
#endif

/* Make sure that at least HUGE_VAL is defined.  */
#ifndef HUGE_VAL
# ifdef HUGE
#  define HUGE_VAL HUGE
# else
#  ifdef MAXFLOAT
#   define HUGE_VAL MAXFLOAT
#  else
#   error "We need HUGE_VAL!"
#  endif
# endif
#endif

#ifndef NAN
# define NAN (nan())
#endif

#ifndef M_PI
# define M_PI 3.14159265358979323846
#endif

/* Test whether number is finite.  */
#ifdef isfinite
/* This is an ISO C 9x function.  */
# define FINITE_P(X) isfinite (X)
# define FINITEF_P(X) isfinite (X)
# define FINITEL_P(X) isfinite (X)
#else
# ifdef IsNANorINF
/* This is for Solaris, which does not have special macros for other
   types.  */
#  define FINITE_P(X) (!IsNANorINF (X))
#  define FINITEF_P(X) (!IsNANorINF (X))
#  define FINITEL_P(X) (!IsNANorINF (X))
# else
#  if defined _GLIBCPP_HAVE_ISINF && defined _GLIBCPP_HAVE_ISNAN
#   define FINITE_P(X) ({ double __x = (X); !isinf (__x) && !isnan (__x); })
#  else
#   ifdef _GLIBCPP_HAVE_FINITE
#    define FINITE_P(X) finite (X)
#   else
#    error "We need FINITE_P"
#   endif
#  endif
#  if defined _GLIBCPP_HAVE_ISINFF && defined _GLIBCPP_HAVE_ISNANF
#   define FINITEF_P(X) ({ float __x = (X); !isinff (__x) && !isnanf (__x); })
#  else
#   ifdef _GLIBCPP_HAVE_FINITE
#    define FINITEF_P(X) finite (X)
#   else
#    define FINITEF_P(X) FINITE_P (X)
#   endif
#  endif
#  if defined _GLIBCPP_HAVE_ISINFL && defined _GLIBCPP_HAVE_ISNANL
#   define FINITEL_P(X) ({ long double __x = (X); \
			   !isinfl (__x) && !isnanl (__x); })
#  else
#   ifdef _GLIBCPP_HAVE_QFINITE
#    define FINITEL_P(X) qfinite (X)
#   else
#    define FINITEL_P(X) FINITE_P (X)
#   endif
#  endif
# endif
#endif

/* Test whether number is infinite.  */
#ifdef isinf
/* This is an ISO C 9x macro.  */
# define INFINITE_P(X) isinf (X)
# define INFINITEF_P(X) isinf (X)
# define INFINITEL_P(X) isinf (X)
#else
# ifdef IsINF
/* This is for Solaris, which does not have special macros for other
   types.  */
#  define INFINITE_P(X) IsINF (X)
#  define INFINITEF_P(X) IsINF (X)
#  define INFINITEL_P(X) IsINF (X)
# else
#  if defined _GLIBCPP_HAVE_ISINF
#   define INFINITE_P(X) isinf (X)
#  else
#   ifdef _GLIBCPP_HAVE_FPCLASS
#    ifdef _FPCLASS_PINF
/* Mingw defines _FPCLASS_PINF.  */
#     define FP_PINF _FPCLASS_PINF
#    endif
/* This is for Irix and Mingw.  */
#    define INFINITE_P(X) (fpclass (fabs (X)) == FP_PINF)
#   else
#    ifdef IS_INF
/* This is for AIX.  */
#     define INFINITE_P(X) ({ double __d = (X); IS_INF (__d); })
#    else
#     error "We need INFINITE_P"
#    endif
#   endif
#  endif
#  if defined _GLIBCPP_HAVE_ISINFF
#   define INFINITEF_P(X) isinff (X)
#  else
#   define INFINITEF_P(X) INFINITE_P (X)
#  endif
#  if defined _GLIBCPP_HAVE_ISINFL
#   define INFINITEL_P(X) isinfl (X)
#  else
#   ifdef _GLIBCPP_HAVE_QFPCLASS
#    define INFINITEL_P(X) (qfpclass (fabsl (X)) == FP_PINF)
#   else
#    define INFINITEL_P(X) INFINITE_P (X)
#   endif
#  endif
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _GLIBCPP_HAVE_COMPLEX_H
# include <complex.h>
#else
# include "complex-stub.h"
#endif
__complex__ double c_log (__complex__ double x);
__complex__ float c_logf (__complex__ float x);
__complex__ long double c_logl (__complex__ long double x);

/* signbit is a macro in ISO C 9x.  */
#ifndef signbit
extern int __signbitf (float);
extern int __signbit (double);
extern int __signbitl (long double);

# define signbit(x) \
     (sizeof (x) == sizeof (float) ?                                          \
        __signbitf (x)                                                        \
      : sizeof (x) == sizeof (double) ?                                       \
        __signbit (x) : __signbitl (x))
#endif

#if BYTE_ORDER == BIG_ENDIAN
typedef union
{
  double value;
  struct
  {
    U_int32_t msw;
    U_int32_t lsw;
  } parts;
} ieee_double_shape_type;
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
typedef union
{
  double value;
  struct
  {
    U_int32_t lsw;
    U_int32_t msw;
  } parts;
} ieee_double_shape_type;
#endif
/* Get the more significant 32 bit int from a double.  */
#define GET_HIGH_WORD(i,d)                                      \
do {                                                            \
  ieee_double_shape_type gh_u;                                  \
  gh_u.value = (d);                                             \
  (i) = gh_u.parts.msw;                                         \
} while (0)


typedef union
{
  float value;
  U_int32_t word;
} ieee_float_shape_type;
/* Get a 32 bit int from a float.  */
#define GET_FLOAT_WORD(i,d)                                     \
do {                                                            \
  ieee_float_shape_type gf_u;                                   \
  gf_u.value = (d);                                             \
  (i) = gf_u.word;                                              \
} while (0)


#if BYTE_ORDER == BIG_ENDIAN
typedef union
{
  long double value;
  struct
  {
    unsigned int sign_exponent:16;
    unsigned int empty:16;
    U_int32_t msw;
    U_int32_t lsw;
  } parts;
} ieee_long_double_shape_type;
#endif
#if BYTE_ORDER == LITTLE_ENDIAN
typedef union
{
  long double value;
  struct
  {
    U_int32_t lsw;
    U_int32_t msw;
    unsigned int sign_exponent:16;
    unsigned int empty:16;
  } parts;
} ieee_long_double_shape_type;
#endif
/* Get int from the exponent of a long double.  */
#define GET_LDOUBLE_EXP(exp,d)                                  \
do {                                                            \
  ieee_long_double_shape_type ge_u;                             \
  ge_u.value = (d);                                             \
  (exp) = ge_u.parts.sign_exponent;                             \
} while (0)


/* Replacement for non-existing float functions.  */
#ifndef _GLIBCPP_HAVE_FABSF
# ifdef __GNUC__
/* gcc has fabsf as a builtin command.  */
extern float fabsf (float);
# else
#  define fabsf(x) fabs (x)
# endif
#endif
#ifndef _GLIBCPP_HAVE_COSF
# define cosf(x) cos (x)
#endif
#ifndef _GLIBCPP_HAVE_COSHF
# define coshf(x) cosh (x)
#endif
#ifndef _GLIBCPP_HAVE_EXPF
# define expf(x) expf (x)
#endif
#ifndef _GLIBCPP_HAVE_LOGF
# define logf(x) log(x)
#endif
#ifndef _GLIBCPP_HAVE_LOG10F
# define log10f(x) log10 (x)
#endif
#ifndef _GLIBCPP_HAVE_POWF
# define powf(x, y) pow (x, y)
#endif
#ifndef _GLIBCPP_HAVE_SINF
# define sinf(x) sin (x)
#endif
#ifndef _GLIBCPP_HAVE_SINHF
# define sinhf(x) sinh (x)
#endif
#ifndef _GLIBCPP_HAVE_SQRTF
# define sqrtf(x) sqrt (x)
#endif
#ifndef _GLIBCPP_HAVE_TANF
# define tanf(x) tan (x)
#endif
#ifndef _GLIBCPP_HAVE_TANHF
# define tanhf(x) tanh (x)
#endif
#ifndef _GLIBCPP_HAVE_STRTOF
# define strtof(s, e) strtod (s, e)
#endif

#ifdef __cplusplus
}
#endif




