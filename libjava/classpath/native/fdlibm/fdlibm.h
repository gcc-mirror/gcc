
/* @(#)fdlibm.h 1.5 04/04/22 */
/*
 * ====================================================
 * Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

#ifndef __CLASSPATH_FDLIBM_H__
#define __CLASSPATH_FDLIBM_H__

/*
 * On AIX we need _ALL_SOURCE defined to compile/configure native-lib, but can't
 *  have it defined to compile fdlibm.  UGH.
 */
#ifdef _AIX
#undef _ALL_SOURCE
#endif

#include <config.h>
#include <stdlib.h>

/*
 * AIX includes a header that defines hz,
 * which conflicts with an fdlibm variable in some functions.
 */
#ifdef _AIX
#undef hz
#endif

/* GCJ LOCAL: Include files.  */
#include "ieeefp.h"
/* CLASSPATH LOCAL: */
#include "namespace.h"

#include "mprec.h"

/* CYGNUS LOCAL: Default to XOPEN_MODE.  */
#define _XOPEN_MODE

#ifdef __P
#undef __P
#endif

/* Sometimes it's necessary to define __LITTLE_ENDIAN explicitly
   but these catch some common cases. */

#if 0
#if defined(i386) || defined(i486) || \
	defined(intel) || defined(x86) || defined(i86pc) || \
	defined(__alpha) || defined(__osf__)
#define __LITTLE_ENDIAN
#endif

#ifdef __LITTLE_ENDIAN
#define __HI(x) *(1+(int*)&x)
#define __LO(x) *(int*)&x
#define __HIp(x) *(1+(int*)x)
#define __LOp(x) *(int*)x
#else
#define __HI(x) *(int*)&x
#define __LO(x) *(1+(int*)&x)
#define __HIp(x) *(int*)x
#define __LOp(x) *(1+(int*)x)
#endif
#endif

#ifdef __STDC__
#define	__P(p)	p
#else
#define	__P(p)	()
#endif

/*
 * ANSI/POSIX
 */

extern int signgam;

#define	MAXFLOAT	((float)3.40282346638528860e+38)

enum fdversion {fdlibm_ieee = -1, fdlibm_svid, fdlibm_xopen, fdlibm_posix};

#define _LIB_VERSION_TYPE enum fdversion
#define _LIB_VERSION _fdlib_version  

/* if global variable _LIB_VERSION is not desirable, one may 
 * change the following to be a constant by: 
 *	#define _LIB_VERSION_TYPE const enum version
 * In that case, after one initializes the value _LIB_VERSION (see
 * s_lib_version.c) during compile time, it cannot be modified
 * in the middle of a program
 */ 
extern  _LIB_VERSION_TYPE  _LIB_VERSION;

#define _IEEE_  fdlibm_ieee
#define _SVID_  fdlibm_svid
#define _XOPEN_ fdlibm_xopen
#define _POSIX_ fdlibm_posix

struct exception {
	int type;
	char *name;
	double arg1;
	double arg2;
	double retval;
};

#define	HUGE		MAXFLOAT

/* 
 * set X_TLOSS = pi*2**52, which is possibly defined in <values.h>
 * (one may replace the following line by "#include <values.h>")
 */

#define X_TLOSS		1.41484755040568800000e+16 

#define	DOMAIN		1
#define	SING		2
#define	OVERFLOW	3
#define	UNDERFLOW	4
#define	TLOSS		5
#define	PLOSS		6

/* These typedefs are true for the targets running Java. */

#define _IEEE_LIBM

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ANSI/POSIX
 */
extern double acos __P((double));
extern double asin __P((double));
extern double atan __P((double));
extern double atan2 __P((double, double));
extern double cos __P((double));
extern double sin __P((double));
extern double tan __P((double));

extern double cosh __P((double));
extern double sinh __P((double));
extern double tanh __P((double));

extern double exp __P((double));
extern double frexp __P((double, int *));
extern double ldexp __P((double, int));
extern double log __P((double));
extern double log10 __P((double));
extern double modf __P((double, double *));

extern double pow __P((double, double));
extern double sqrt __P((double));

extern double ceil __P((double));
extern double fabs __P((double));
extern double floor __P((double));
extern double fmod __P((double, double));

extern double erf __P((double));
extern double erfc __P((double));
extern double gamma __P((double));
extern double hypot __P((double, double));

#if !defined(isnan)
#define isnan(x) ((x) != (x))
#endif

extern int finite __P((double));
extern double j0 __P((double));
extern double j1 __P((double));
extern double jn __P((int, double));
extern double lgamma __P((double));
extern double y0 __P((double));
extern double y1 __P((double));
extern double yn __P((int, double));

extern double acosh __P((double));
extern double asinh __P((double));
extern double atanh __P((double));
extern double cbrt __P((double));
extern double logb __P((double));
extern double nextafter __P((double, double));
extern double remainder __P((double, double));
#ifdef _SCALB_INT
extern double scalb __P((double, int));
#else
extern double scalb __P((double, double));
#endif

extern int matherr __P((struct exception *));

/*
 * IEEE Test Vector
 */
extern double significand __P((double));

/*
 * Functions callable from C, intended to support IEEE arithmetic.
 */
extern double copysign __P((double, double));
extern int ilogb __P((double));
extern double rint __P((double));
extern double scalbn __P((double, int));

/*
 * BSD math library entry points
 */
extern double expm1 __P((double));
extern double log1p __P((double));

/*
 * Reentrant version of gamma & lgamma; passes signgam back by reference
 * as the second argument; user must allocate space for signgam.
 */
#ifdef _REENTRANT
extern double gamma_r __P((double, int *));
extern double lgamma_r __P((double, int *));
#endif	/* _REENTRANT */

/* ieee style elementary functions */
extern double __ieee754_sqrt __P((double));			
extern double __ieee754_acos __P((double));			
extern double __ieee754_acosh __P((double));			
extern double __ieee754_log __P((double));			
extern double __ieee754_atanh __P((double));			
extern double __ieee754_asin __P((double));			
extern double __ieee754_atan2 __P((double,double));			
extern double __ieee754_exp __P((double));
extern double __ieee754_cosh __P((double));
extern double __ieee754_fmod __P((double,double));
extern double __ieee754_pow __P((double,double));
extern double __ieee754_lgamma_r __P((double,int *));
extern double __ieee754_gamma_r __P((double,int *));
extern double __ieee754_lgamma __P((double));
extern double __ieee754_gamma __P((double));
extern double __ieee754_log10 __P((double));
extern double __ieee754_sinh __P((double));
extern double __ieee754_hypot __P((double,double));
extern double __ieee754_j0 __P((double));
extern double __ieee754_j1 __P((double));
extern double __ieee754_y0 __P((double));
extern double __ieee754_y1 __P((double));
extern double __ieee754_jn __P((int,double));
extern double __ieee754_yn __P((int,double));
extern double __ieee754_remainder __P((double,double));
extern int32_t    __ieee754_rem_pio2 __P((double,double*));
#ifdef _SCALB_INT
extern double __ieee754_scalb __P((double,int));
#else
extern double __ieee754_scalb __P((double,double));
#endif

/* fdlibm kernel function */
extern double __kernel_standard __P((double,double,int));	
extern double __kernel_sin __P((double,double,int));
extern double __kernel_cos __P((double,double));
extern double __kernel_tan __P((double,double,int));
extern int    __kernel_rem_pio2 __P((double*,double*,int,int,int,const int*));

/* Classpath extensions */

/* The original code used statements like  
   n0 = ((*(int*)&one)>>29)^1;             * index of high word *  
   ix0 = *(n0+(int*)&x);                   * high word of x *  
   ix1 = *((1-n0)+(int*)&x);               * low word of x *  
   to dig two 32 bit words out of the 64 bit IEEE floating point  
   value.  That is non-ANSI, and, moreover, the gcc instruction  
   scheduler gets it wrong.  We instead use the following macros.  
   Unlike the original code, we determine the endianness at compile  
   time, not at run time; I don't see much benefit to selecting  
   endianness at run time.  */  
   
#ifndef __IEEE_BIG_ENDIAN  
#ifndef __IEEE_LITTLE_ENDIAN  
#error Must define endianness  
#endif  
#endif  
   
/* A union which permits us to convert between a double and two 32 bit  
   ints.  */  
   
#ifdef __IEEE_BIG_ENDIAN  
   
  typedef union  
  {  
    double value;  
    struct  
    {  
      uint32_t msw;  
      uint32_t lsw;  
    } parts;  
  } ieee_double_shape_type;  
   
#endif  
   
#ifdef __IEEE_LITTLE_ENDIAN  
   
  typedef union  
  {  
    double value;  
    struct  
    {  
      uint32_t lsw;  
      uint32_t msw;  
    } parts;  
  } ieee_double_shape_type;  
   
#endif  
   
  /* Get two 32 bit ints from a double.  */  
   
#define EXTRACT_WORDS(ix0,ix1,d)                                  \
  do {                                                            \
    ieee_double_shape_type ew_u;                                  \
    ew_u.value = (d);                                             \
    (ix0) = ew_u.parts.msw;                                       \
    (ix1) = ew_u.parts.lsw;                                       \
  } while (0)  
        
/* Get the more significant 32 bit int from a double.  */  
        
#define GET_HIGH_WORD(i,d)                                      \
  do {                                                            \
    ieee_double_shape_type gh_u;                                  \
    gh_u.value = (d);                                             \
    (i) = gh_u.parts.msw;                                         \
  } while (0)  
        
/* Get the less significant 32 bit int from a double.  */  
        
#define GET_LOW_WORD(i,d)                                       \
  do {                                                            \
    ieee_double_shape_type gl_u;                                  \
    gl_u.value = (d);                                             \
    (i) = gl_u.parts.lsw;                                         \
  } while (0)  
        
/* Set a double from two 32 bit ints.  */  
        
#define INSERT_WORDS(d,ix0,ix1)                                 \
  do {                                                            \
    ieee_double_shape_type iw_u;                                  \
    iw_u.parts.msw = (ix0);                                       \
    iw_u.parts.lsw = (ix1);                                       \
    (d) = iw_u.value;                                             \
  } while (0)  
        
/* Set the more significant 32 bits of a double from an int.  */  
        
#define SET_HIGH_WORD(d,v)                                      \
  do {                                                            \
    ieee_double_shape_type sh_u;                                  \
    sh_u.value = (d);                                             \
    sh_u.parts.msw = (v);                                         \
    (d) = sh_u.value;                                             \
  } while (0)  
        
/* Set the less significant 32 bits of a double from an int.  */  
        
#define SET_LOW_WORD(d,v)                                       \
  do {                                                            \
    ieee_double_shape_type sl_u;                                  \
    sl_u.value = (d);                                             \
    sl_u.parts.lsw = (v);                                         \
    (d) = sl_u.value;                                             \
  } while (0)  

/* A union which permits us to convert between a float and a 32 bit
   int.  */

typedef union
{
  float value;
  uint32_t word;
} ieee_float_shape_type;

/* Get a 32 bit int from a float.  */

#define GET_FLOAT_WORD(i,d)					\
do {								\
  ieee_float_shape_type gf_u;					\
  gf_u.value = (d);						\
  (i) = gf_u.word;						\
} while (0)

/* Set a float from a 32 bit int.  */

#define SET_FLOAT_WORD(d,i)					\
do {								\
  ieee_float_shape_type sf_u;					\
  sf_u.word = (i);						\
  (d) = sf_u.value;						\
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* __CLASSPATH_FDLIBM_H__ */

