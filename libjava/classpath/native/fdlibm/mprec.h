/****************************************************************
 *
 * The author of this software is David M. Gay.
 *
 * Copyright (c) 1991, 2000 by AT&T.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR AT&T MAKES ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 *
 ***************************************************************/

/* Please send bug reports to
	David M. Gay
	AT&T Bell Laboratories, Room 2C-463
	600 Mountain Avenue
	Murray Hill, NJ 07974-2070
	U.S.A.
	dmg@research.att.com or research!dmg
 */

#ifndef __CLASSPATH_MPREC_H__
#define __CLASSPATH_MPREC_H__

#include <config.h>
#include "config-int.h"
#include "ieeefp.h"
/* CLASSPATH LOCAL */
#include "namespace.h"

#if defined HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if defined HAVE_SYS_CONFIG_H
#include <sys/config.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

  /* These typedefs are true for the targets running Java. */

#ifdef __IEEE_LITTLE_ENDIAN
#define IEEE_8087
#endif

#ifdef __IEEE_BIG_ENDIAN
#define IEEE_MC68k
#endif

#ifdef __Z8000__
#define Just_16
#endif

#ifdef DEBUG
#include "stdio.h"
#include <stdlib.h>
#define Bug(x) {fprintf(stderr, "%s\n", x); exit(1);}
#endif


#ifdef Unsigned_Shifts
#define Sign_Extend(a,b) if (b < 0) a |= (uint32_t)0xffff0000;
#else
#define Sign_Extend(a,b) /*no-op*/
#endif

#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) != 1
Exactly one of IEEE_8087, IEEE_MC68k, VAX, or IBM should be defined.
#endif

/* If we are going to examine or modify specific bits in a double using
   the word0 and/or word1 macros, then we must wrap the double inside
   a union.  This is necessary to avoid undefined behavior according to
   the ANSI C spec.  */
union double_union
{
  double d;
  uint32_t i[2];
};

#ifdef IEEE_8087
#define word0(x) (x.i[1])
#define word1(x) (x.i[0])
#else
#define word0(x) (x.i[0])
#define word1(x) (x.i[1])
#endif

/* The following definition of Storeinc is appropriate for MIPS processors.
 * An alternative that might be better on some machines is
 * #define Storeinc(a,b,c) (*a++ = b << 16 | c & 0xffff)
 */
#if defined(IEEE_8087) + defined(VAX)
#define Storeinc(a,b,c) (((unsigned short *)a)[1] = (unsigned short)b, \
((unsigned short *)a)[0] = (unsigned short)c, a++)
#else
#define Storeinc(a,b,c) (((unsigned short *)a)[0] = (unsigned short)b, \
((unsigned short *)a)[1] = (unsigned short)c, a++)
#endif

/* #define P DBL_MANT_DIG */
/* Ten_pmax = floor(P*log(2)/log(5)) */
/* Bletch = (highest power of 2 < DBL_MAX_10_EXP) / 16 */
/* Quick_max = floor((P-1)*log(FLT_RADIX)/log(10) - 1) */
/* Int_max = floor(P*log(FLT_RADIX)/log(10) - 1) */

#if defined(IEEE_8087) + defined(IEEE_MC68k)
#if defined (_DOUBLE_IS_32BITS)
#define Exp_shift   23
#define Exp_shift1  23
#define Exp_msk1    ((uint32_t)0x00800000L)
#define Exp_msk11   ((uint32_t)0x00800000L)
#define Exp_mask    ((uint32_t)0x7f800000L)
#define P    	    24
#define Bias 	    127
#if 0
#define IEEE_Arith  /* it is, but the code doesn't handle IEEE singles yet */
#endif
#define Emin        (-126)
#define Exp_1       ((uint32_t)0x3f800000L)
#define Exp_11      ((uint32_t)0x3f800000L)
#define Ebits 	    8
#define Frac_mask   ((uint32_t)0x007fffffL)
#define Frac_mask1  ((uint32_t)0x007fffffL)
#define Ten_pmax    10
#define Sign_bit    ((uint32_t)0x80000000L)
#define Ten_pmax    10
#define Bletch	    2
#define Bndry_mask  ((uint32_t)0x007fffffL)
#define Bndry_mask1 ((uint32_t)0x007fffffL)
#define LSB 1
#define Sign_bit    ((uint32_t)0x80000000L)
#define Log2P 	    1
#define Tiny0 	    0
#define Tiny1 	    1
#define Quick_max   5
#define Int_max     6
#define Infinite(x) (word0(x) == ((uint32_t)0x7f800000L))
#undef word0
#undef word1

#define word0(x) (x.i[0])
#define word1(x) 0
#else

#define Exp_shift  20
#define Exp_shift1 20
#define Exp_msk1    ((uint32_t)0x100000L)
#define Exp_msk11   ((uint32_t)0x100000L)
#define Exp_mask  ((uint32_t)0x7ff00000L)
#define P 53
#define Bias 1023
#define IEEE_Arith
#define Emin (-1022)
#define Exp_1  ((uint32_t)0x3ff00000L)
#define Exp_11 ((uint32_t)0x3ff00000L)
#define Ebits 11
#define Frac_mask  ((uint32_t)0xfffffL)
#define Frac_mask1 ((uint32_t)0xfffffL)
#define Ten_pmax 22
#define Bletch 0x10
#define Bndry_mask  ((uint32_t)0xfffffL)
#define Bndry_mask1 ((uint32_t)0xfffffL)
#define LSB 1
#define Sign_bit ((uint32_t)0x80000000L)
#define Log2P 1
#define Tiny0 0
#define Tiny1 1
#define Quick_max 14
#define Int_max 14
#define Infinite(x) (word0(x) == ((uint32_t)0x7ff00000L)) /* sufficient test for here */
#endif

#else
#undef  Sudden_Underflow
#define Sudden_Underflow
#ifdef IBM
#define Exp_shift  24
#define Exp_shift1 24
#define Exp_msk1   ((uint32_t)0x1000000L)
#define Exp_msk11  ((uint32_t)0x1000000L)
#define Exp_mask  ((uint32_t)0x7f000000L)
#define P 14
#define Bias 65
#define Exp_1  ((uint32_t)0x41000000L)
#define Exp_11 ((uint32_t)0x41000000L)
#define Ebits 8	/* exponent has 7 bits, but 8 is the right value in b2d */
#define Frac_mask  ((uint32_t)0xffffffL)
#define Frac_mask1 ((uint32_t)0xffffffL)
#define Bletch 4
#define Ten_pmax 22
#define Bndry_mask  ((uint32_t)0xefffffL)
#define Bndry_mask1 ((uint32_t)0xffffffL)
#define LSB 1
#define Sign_bit ((uint32_t)0x80000000L)
#define Log2P 4
#define Tiny0 ((uint32_t)0x100000L)
#define Tiny1 0
#define Quick_max 14
#define Int_max 15
#else /* VAX */
#define Exp_shift  23
#define Exp_shift1 7
#define Exp_msk1    0x80
#define Exp_msk11   ((uint32_t)0x800000L)
#define Exp_mask  ((uint32_t)0x7f80L)
#define P 56
#define Bias 129
#define Exp_1  ((uint32_t)0x40800000L)
#define Exp_11 ((uint32_t)0x4080L)
#define Ebits 8
#define Frac_mask  ((uint32_t)0x7fffffL)
#define Frac_mask1 ((uint32_t)0xffff007fL)
#define Ten_pmax 24
#define Bletch 2
#define Bndry_mask  ((uint32_t)0xffff007fL)
#define Bndry_mask1 ((uint32_t)0xffff007fL)
#define LSB ((uint32_t)0x10000L)
#define Sign_bit ((uint32_t)0x8000L)
#define Log2P 1
#define Tiny0 0x80
#define Tiny1 0
#define Quick_max 15
#define Int_max 15
#endif
#endif

#ifndef IEEE_Arith
#define ROUND_BIASED
#endif

#ifdef RND_PRODQUOT
#define rounded_product(a,b) a = rnd_prod(a, b)
#define rounded_quotient(a,b) a = rnd_quot(a, b)
#ifdef KR_headers
extern double rnd_prod(), rnd_quot();
#else
extern double rnd_prod(double, double), rnd_quot(double, double);
#endif
#else
#define rounded_product(a,b) a *= b
#define rounded_quotient(a,b) a /= b
#endif

#define Big0 (Frac_mask1 | Exp_msk1*(DBL_MAX_EXP+Bias-1))
#define Big1 ((uint32_t)0xffffffffL)

#ifndef Just_16
/* When Pack_32 is not defined, we store 16 bits per 32-bit long.
 * This makes some inner loops simpler and sometimes saves work
 * during multiplications, but it often seems to make things slightly
 * slower.  Hence the default is now to store 32 bits per long.
 */

#ifndef Pack_32
#if SIZEOF_VOID_P != 8
#define Pack_32
#endif
#endif
#endif


#define MAX_BIGNUMS 16
#ifdef Pack_32
#define MAX_BIGNUM_WDS 32
#else
  /* Note that this is a workaround for */
#define MAX_BIGNUM_WDS 128
#endif

struct _Jv_Bigint
{
  struct _Jv_Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  unsigned long _x[MAX_BIGNUM_WDS];
};


#define	_PTR		void *
#define	_AND		,
#define	_NOARGS		void
#define	_CONST		const
#define	_VOLATILE	volatile
#define	_SIGNED		signed
#define	_DOTS		, ...
#define _VOID void
#define	_EXFUN(name, proto)		name proto
#define	_DEFUN(name, arglist, args)	name(args)
#define	_DEFUN_VOID(name)		name(_NOARGS)
#define _CAST_VOID (void)


struct _Jv_reent
{
  /* local copy of errno */
  int _errno;

  /* used by mprec routines */
  struct _Jv_Bigint *_result;
  int _result_k;
  struct _Jv_Bigint *_p5s;

  struct _Jv_Bigint _freelist[MAX_BIGNUMS];
  int _allocation_map;

  int num;
};


typedef struct _Jv_Bigint _Jv_Bigint;

#define Balloc  _Jv_Balloc
#define Bfree   _Jv_Bfree
#define multadd _Jv_multadd
#define s2b     _Jv_s2b
#define lo0bits _Jv_lo0bits
#define hi0bits _Jv_hi0bits
#define i2b     _Jv_i2b
#define mult    _Jv_mult
#define pow5mult        _Jv_pow5mult
#define lshift  _Jv_lshift
#define cmp     _Jv__mcmp
#define diff    _Jv__mdiff
#define ulp     _Jv_ulp
#define b2d     _Jv_b2d
#define d2b     _Jv_d2b
#define ratio   _Jv_ratio

#define tens _Jv__mprec_tens
#define bigtens _Jv__mprec_bigtens
#define tinytens _Jv__mprec_tinytens

#define _dtoa _Jv_dtoa
#define _dtoa_r _Jv_dtoa_r
#define _strtod_r _Jv_strtod_r

extern double _EXFUN(_strtod_r, (struct _Jv_reent *ptr, const char *s00, char **se));
extern char* _EXFUN(_dtoa_r, (struct _Jv_reent *ptr, double d,
			      int mode, int ndigits, int *decpt, int *sign,
			      char **rve, int float_type));
void _EXFUN(_dtoa, (double d, int mode, int ndigits, int *decpt, int *sign,
		    char **rve, char *buf, int float_type));

double 		_EXFUN(ulp,(double x));
double		_EXFUN(b2d,(_Jv_Bigint *a , int *e));
_Jv_Bigint *	_EXFUN(Balloc,(struct _Jv_reent *p, int k));
void 		_EXFUN(Bfree,(struct _Jv_reent *p, _Jv_Bigint *v));
_Jv_Bigint *	_EXFUN(multadd,(struct _Jv_reent *p, _Jv_Bigint *, int, int));
_Jv_Bigint *	_EXFUN(s2b,(struct _Jv_reent *, const char*, int, int, unsigned long));
_Jv_Bigint *	_EXFUN(i2b,(struct _Jv_reent *,int));
_Jv_Bigint *	_EXFUN(mult, (struct _Jv_reent *, _Jv_Bigint *, _Jv_Bigint *));
_Jv_Bigint *	_EXFUN(pow5mult, (struct _Jv_reent *, _Jv_Bigint *, int k));
int 		_EXFUN(hi0bits,(unsigned long));
int 		_EXFUN(lo0bits,(unsigned long *));
_Jv_Bigint *    _EXFUN(d2b,(struct _Jv_reent *p, double d, int *e, int *bits));
_Jv_Bigint *    _EXFUN(lshift,(struct _Jv_reent *p, _Jv_Bigint *b, int k));
_Jv_Bigint *    _EXFUN(diff,(struct _Jv_reent *p, _Jv_Bigint *a, _Jv_Bigint *b));
int             _EXFUN(cmp,(_Jv_Bigint *a, _Jv_Bigint *b));

double		_EXFUN(ratio,(_Jv_Bigint *a, _Jv_Bigint *b));
#define Bcopy(x,y) memcpy((char *)&x->_sign, (char *)&y->_sign, y->_wds*sizeof(long) + 2*sizeof(int))

#if defined(_DOUBLE_IS_32BITS) && defined(__v800)
#define n_bigtens 2
#else
#define n_bigtens 5
#endif

extern _CONST double tinytens[];
extern _CONST double bigtens[];
extern _CONST double tens[];

#ifdef __cplusplus
}
#endif

#endif /* __CLASSPATH_MPREC_H__ */
