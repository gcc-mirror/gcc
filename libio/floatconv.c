/* 
Copyright (C) 1993, 1994 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

#include <libioP.h>
#ifdef _IO_USE_DTOA
/****************************************************************
 *
 * The author of this software is David M. Gay.
 *
 * Copyright (c) 1991 by AT&T.
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

/* Some cleaning up by Per Bothner, bothner@cygnus.com, 1992, 1993.
   Re-written to not need static variables
   (except result, result_k, HIWORD, LOWORD). */

/* Note that the checking of _DOUBLE_IS_32BITS is for use with the
   cross targets that employ the newlib ieeefp.h header.  -- brendan */

/* Please send bug reports to
        David M. Gay
        AT&T Bell Laboratories, Room 2C-463
        600 Mountain Avenue
        Murray Hill, NJ 07974-2070
        U.S.A.
        dmg@research.att.com or research!dmg
 */

/* strtod for IEEE-, VAX-, and IBM-arithmetic machines.
 *
 * This strtod returns a nearest machine number to the input decimal
 * string (or sets errno to ERANGE).  With IEEE arithmetic, ties are
 * broken by the IEEE round-even rule.  Otherwise ties are broken by
 * biased rounding (add half and chop).
 *
 * Inspired loosely by William D. Clinger's paper "How to Read Floating
 * Point Numbers Accurately" [Proc. ACM SIGPLAN '90, pp. 92-101].
 *
 * Modifications:
 *
 *      1. We only require IEEE, IBM, or VAX double-precision
 *              arithmetic (not IEEE double-extended).
 *      2. We get by with floating-point arithmetic in a case that
 *              Clinger missed -- when we're computing d * 10^n
 *              for a small integer d and the integer n is not too
 *              much larger than 22 (the maximum integer k for which
 *              we can represent 10^k exactly), we may be able to
 *              compute (d*10^k) * 10^(e-k) with just one roundoff.
 *      3. Rather than a bit-at-a-time adjustment of the binary
 *              result in the hard case, we use floating-point
 *              arithmetic to determine the adjustment to within
 *              one bit; only in really hard cases do we need to
 *              compute a second residual.
 *      4. Because of 3., we don't need a large table of powers of 10
 *              for ten-to-e (just some small tables, e.g. of 10^k
 *              for 0 <= k <= 22).
 */

/*
 * #define IEEE_8087 for IEEE-arithmetic machines where the least
 *      significant byte has the lowest address.
 * #define IEEE_MC68k for IEEE-arithmetic machines where the most
 *      significant byte has the lowest address.
 * #define Sudden_Underflow for IEEE-format machines without gradual
 *      underflow (i.e., that flush to zero on underflow).
 * #define IBM for IBM mainframe-style floating-point arithmetic.
 * #define VAX for VAX-style floating-point arithmetic.
 * #define Unsigned_Shifts if >> does treats its left operand as unsigned.
 * #define No_leftright to omit left-right logic in fast floating-point
 *      computation of dtoa.
 * #define Check_FLT_ROUNDS if FLT_ROUNDS can assume the values 2 or 3.
 * #define RND_PRODQUOT to use rnd_prod and rnd_quot (assembly routines
 *      that use extended-precision instructions to compute rounded
 *      products and quotients) with IBM.
 * #define ROUND_BIASED for IEEE-format with biased rounding.
 * #define Inaccurate_Divide for IEEE-format with correctly rounded
 *      products but inaccurate quotients, e.g., for Intel i860.
 * #define KR_headers for old-style C function headers.
 */

#ifdef DEBUG
#include <stdio.h>
#define Bug(x) {fprintf(stderr, "%s\n", x); exit(1);}
#endif

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#include <float.h>
#define CONST const
#else
#define CONST
#define KR_headers

/* In this case, we assume IEEE floats. */
#define FLT_ROUNDS 1
#define FLT_RADIX 2
#define DBL_MANT_DIG 53
#define DBL_DIG 15
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP 1024
#endif

#include <errno.h>
#ifndef __MATH_H__
#include <math.h>
#endif

#ifdef Unsigned_Shifts
#define Sign_Extend(a,b) if (b < 0) a |= 0xffff0000;
#else
#define Sign_Extend(a,b) /*no-op*/
#endif

#if defined(__i386__) || defined(__i860__) || defined(clipper)
#define IEEE_8087
#endif
#if defined(MIPSEL) || defined(__alpha__)
#define IEEE_8087
#endif
#if defined(__sparc__) || defined(sparc) || defined(MIPSEB)
#define IEEE_MC68k
#endif

#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) != 1

#ifndef _DOUBLE_IS_32BITS
#if FLT_RADIX==16
#define IBM
#else
#if DBL_MANT_DIG==56
#define VAX
#else
#if DBL_MANT_DIG==53 && DBL_MAX_10_EXP==308
#define IEEE_Unknown
#else
Exactly one of IEEE_8087, IEEE_MC68k, VAX, or IBM should be defined.
#endif
#endif
#endif
#endif /* !_DOUBLE_IS_32BITS */
#endif

typedef _G_uint32_t unsigned32;

union doubleword {
  double d;
  unsigned32 u[2];
};

#ifdef IEEE_8087
#define HIWORD 1
#define LOWORD 0
#define TEST_ENDIANNESS  /* nothing */
#else
#if defined(IEEE_MC68k)
#define HIWORD 0
#define LOWORD 1
#define TEST_ENDIANNESS  /* nothing */
#else
static int HIWORD = -1, LOWORD;
static void test_endianness()
{
    union doubleword dw;
    dw.d = 10;
    if (dw.u[0] != 0) /* big-endian */
	HIWORD=0, LOWORD=1;
    else
	HIWORD=1, LOWORD=0;
}
#define TEST_ENDIANNESS  if (HIWORD<0) test_endianness();
#endif
#endif

#if 0
union doubleword _temp;
#endif
#if defined(__GNUC__) && !defined(_DOUBLE_IS_32BITS)
#define word0(x) ({ union doubleword _du; _du.d = (x); _du.u[HIWORD]; })
#define word1(x) ({ union doubleword _du; _du.d = (x); _du.u[LOWORD]; })
#define setword0(D,W) \
  ({ union doubleword _du; _du.d = (D); _du.u[HIWORD]=(W); (D)=_du.d; })
#define setword1(D,W) \
  ({ union doubleword _du; _du.d = (D); _du.u[LOWORD]=(W); (D)=_du.d; })
#define setwords(D,W0,W1) ({ union doubleword _du; \
  _du.u[HIWORD]=(W0); _du.u[LOWORD]=(W1); (D)=_du.d; })
#define addword0(D,W) \
  ({ union doubleword _du; _du.d = (D); _du.u[HIWORD]+=(W); (D)=_du.d; })
#else
#define word0(x) ((unsigned32 *)&x)[HIWORD]
#ifndef _DOUBLE_IS_32BITS
#define word1(x) ((unsigned32 *)&x)[LOWORD]
#else
#define word1(x) 0
#endif
#define setword0(D,W) word0(D) = (W)
#ifndef _DOUBLE_IS_32BITS
#define setword1(D,W) word1(D) = (W)
#define setwords(D,W0,W1) (setword0(D,W0),setword1(D,W1))
#else
#define setword1(D,W)
#define setwords(D,W0,W1) (setword0(D,W0))
#endif
#define addword0(D,X) (word0(D) += (X))
#endif

/* The following definition of Storeinc is appropriate for MIPS processors. */
#if defined(IEEE_8087) + defined(VAX)
#define Storeinc(a,b,c) (((unsigned short *)a)[1] = (unsigned short)b, \
((unsigned short *)a)[0] = (unsigned short)c, a++)
#else
#if defined(IEEE_MC68k)
#define Storeinc(a,b,c) (((unsigned short *)a)[0] = (unsigned short)b, \
((unsigned short *)a)[1] = (unsigned short)c, a++)
#else
#define Storeinc(a,b,c) (*a++ = b << 16 | c & 0xffff)
#endif
#endif

/* #define P DBL_MANT_DIG */
/* Ten_pmax = floor(P*log(2)/log(5)) */
/* Bletch = (highest power of 2 < DBL_MAX_10_EXP) / 16 */
/* Quick_max = floor((P-1)*log(FLT_RADIX)/log(10) - 1) */
/* Int_max = floor(P*log(FLT_RADIX)/log(10) - 1) */

#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(IEEE_Unknown)
#define Exp_shift  20
#define Exp_shift1 20
#define Exp_msk1    0x100000
#define Exp_msk11   0x100000
#define Exp_mask  0x7ff00000
#define P 53
#define Bias 1023
#define IEEE_Arith
#define Emin (-1022)
#define Exp_1  0x3ff00000
#define Exp_11 0x3ff00000
#define Ebits 11
#define Frac_mask  0xfffff
#define Frac_mask1 0xfffff
#define Ten_pmax 22
#define Bletch 0x10
#define Bndry_mask  0xfffff
#define Bndry_mask1 0xfffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 1
#define Tiny0 0
#define Tiny1 1
#define Quick_max 14
#define Int_max 14
#define Infinite(x) (word0(x) == 0x7ff00000) /* sufficient test for here */
#else
#undef  Sudden_Underflow
#define Sudden_Underflow
#ifdef IBM
#define Exp_shift  24
#define Exp_shift1 24
#define Exp_msk1   0x1000000
#define Exp_msk11  0x1000000
#define Exp_mask  0x7f000000
#define P 14
#define Bias 65
#define Exp_1  0x41000000
#define Exp_11 0x41000000
#define Ebits 8 /* exponent has 7 bits, but 8 is the right value in b2d */
#define Frac_mask  0xffffff
#define Frac_mask1 0xffffff
#define Bletch 4
#define Ten_pmax 22
#define Bndry_mask  0xefffff
#define Bndry_mask1 0xffffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 4
#define Tiny0 0x100000
#define Tiny1 0
#define Quick_max 14
#define Int_max 15
#else /* VAX */
#define Exp_shift  23
#define Exp_shift1 7
#define Exp_msk1    0x80
#define Exp_msk11   0x800000
#define Exp_mask  0x7f80
#define P 56
#define Bias 129
#define Exp_1  0x40800000
#define Exp_11 0x4080
#define Ebits 8
#define Frac_mask  0x7fffff
#define Frac_mask1 0xffff007f
#define Ten_pmax 24
#define Bletch 2
#define Bndry_mask  0xffff007f
#define Bndry_mask1 0xffff007f
#define LSB 0x10000
#define Sign_bit 0x8000
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
extern double rnd_prod(double, double), rnd_quot(double, double);
#else
#define rounded_product(a,b) a *= b
#define rounded_quotient(a,b) a /= b
#endif

#define Big0 (Frac_mask1 | Exp_msk1*(DBL_MAX_EXP+Bias-1))
#define Big1 0xffffffff

#define Kmax 15

/* (1<<BIGINT_MINIMUM_K) is the minimum number of words to allocate
   in a Bigint.  dtoa usually manages with 1<<2, and has not been
   known to need more than 1<<3.  */

#define BIGINT_MINIMUM_K 3

struct Bigint {
  struct Bigint *next;
  int k;		/* Parameter given to Balloc(k) */
  int maxwds;		/* Allocated space: equals 1<<k. */
  short on_stack;	/* 1 if stack-allocated. */
  short sign;		/* 0 if value is positive or zero; 1 if negative. */
  int wds;		/* Current length. */
  unsigned32 x[1<<BIGINT_MINIMUM_K]; /* Actually: x[maxwds] */
};

#define BIGINT_HEADER_SIZE \
  (sizeof(Bigint) - (1<<BIGINT_MINIMUM_K) * sizeof(unsigned32))

typedef struct Bigint Bigint;

/* Initialize a stack-allocated Bigint. */

static Bigint *
Binit
#ifdef KR_headers
        (v) Bigint *v;
#else
        (Bigint *v)
#endif
{
  v->on_stack = 1;
  v->k = BIGINT_MINIMUM_K;
  v->maxwds = 1 << BIGINT_MINIMUM_K;
  v->sign = v->wds = 0;
  return v;
}

/* Allocate a Bigint with '1<<k' big digits. */

static Bigint *
Balloc
#ifdef KR_headers
        (k) int k;
#else
        (int k)
#endif
{
  int x;
  Bigint *rv;

  if (k < BIGINT_MINIMUM_K)
    k = BIGINT_MINIMUM_K;

  x = 1 << k;
  rv = (Bigint *)
    malloc(BIGINT_HEADER_SIZE + x * sizeof(unsigned32));
  rv->k = k;
  rv->maxwds = x;
  rv->sign = rv->wds = 0;
  rv->on_stack = 0;
  return rv;
}

static void
Bfree
#ifdef KR_headers
        (v) Bigint *v;
#else
        (Bigint *v)
#endif
{
  if (v && !v->on_stack)
    free (v);
}

static void
Bcopy
#ifdef KR_headers
        (x, y) Bigint *x, *y;
#else
        (Bigint *x, Bigint *y)
#endif
{
  register unsigned32 *xp, *yp;
  register int i = y->wds;
  x->sign = y->sign;
  x->wds = i;
  for (xp = x->x, yp = y->x; --i >= 0; )
    *xp++ = *yp++;
}

/* Make sure b has room for at least 1<<k big digits. */

static Bigint *
Brealloc
#ifdef KR_headers
        (b, k) Bigint *b; int k;
#else
        (Bigint * b, int k)
#endif
{
  if (b == NULL)
    return Balloc(k);
  if (b->k >= k)
    return b;
  else
    {
      Bigint *rv = Balloc (k);
      Bcopy(rv, b);
      Bfree(b);
      return rv;
    }
}

/* Return b*m+a.  b is modified.
   Assumption:  0xFFFF*m+a fits in 32 bits. */

static Bigint *
multadd
#ifdef KR_headers
        (b, m, a) Bigint *b; int m, a;
#else
        (Bigint *b, int m, int a)
#endif
{
        int i, wds;
        unsigned32 *x, y;
        unsigned32 xi, z;

        wds = b->wds;
        x = b->x;
        i = 0;
        do {
                xi = *x;
                y = (xi & 0xffff) * m + a;
                z = (xi >> 16) * m + (y >> 16);
                a = (int)(z >> 16);
                *x++ = (z << 16) + (y & 0xffff);
                }
                while(++i < wds);
        if (a) {
                if (wds >= b->maxwds)
                        b = Brealloc(b, b->k+1);
                b->x[wds++] = a;
                b->wds = wds;
                }
        return b;
        }

static Bigint *
s2b
#ifdef KR_headers
        (result, s, nd0, nd, y9)
	Bigint *result; CONST char *s; int nd0, nd; unsigned32 y9;
#else
        (Bigint *result, CONST char *s, int nd0, int nd, unsigned32 y9)
#endif
{
  int i, k;
  _G_int32_t x, y;

  x = (nd + 8) / 9;
  for(k = 0, y = 1; x > y; y <<= 1, k++) ;
  result = Brealloc(result, k);
  result->x[0] = y9;
  result->wds = 1;

  i = 9;
  if (9 < nd0)
    {
      s += 9;
      do
	result = multadd(result, 10, *s++ - '0');
      while (++i < nd0);
      s++;
    }
  else
    s += 10;
  for(; i < nd; i++)
    result = multadd(result, 10, *s++ - '0');
  return result;
}

static int
hi0bits
#ifdef KR_headers
        (x) register unsigned32 x;
#else
        (register unsigned32 x)
#endif
{
        register int k = 0;

        if (!(x & 0xffff0000)) {
                k = 16;
                x <<= 16;
                }
        if (!(x & 0xff000000)) {
                k += 8;
                x <<= 8;
                }
        if (!(x & 0xf0000000)) {
                k += 4;
                x <<= 4;
                }
        if (!(x & 0xc0000000)) {
                k += 2;
                x <<= 2;
                }
        if (!(x & 0x80000000)) {
                k++;
                if (!(x & 0x40000000))
                        return 32;
                }
        return k;
        }

static int
lo0bits
#ifdef KR_headers
        (y) unsigned32 *y;
#else
        (unsigned32 *y)
#endif
{
        register int k;
        register unsigned32 x = *y;

        if (x & 7) {
                if (x & 1)
                        return 0;
                if (x & 2) {
                        *y = x >> 1;
                        return 1;
                        }
                *y = x >> 2;
                return 2;
                }
        k = 0;
        if (!(x & 0xffff)) {
                k = 16;
                x >>= 16;
                }
        if (!(x & 0xff)) {
                k += 8;
                x >>= 8;
                }
        if (!(x & 0xf)) {
                k += 4;
                x >>= 4;
                }
        if (!(x & 0x3)) {
                k += 2;
                x >>= 2;
                }
        if (!(x & 1)) {
                k++;
                x >>= 1;
                if (!x & 1)
                        return 32;
                }
        *y = x;
        return k;
        }

static Bigint *
i2b
#ifdef KR_headers
        (result, i) Bigint *result; int i;
#else
        (Bigint* result, int i)
#endif
{
  result = Brealloc(result, 1);
  result->x[0] = i;
  result->wds = 1;
  return result;
}

/* Do: c = a * b. */

static Bigint *
mult
#ifdef KR_headers
        (c, a, b) Bigint *a, *b, *c;
#else
        (Bigint *c, Bigint *a, Bigint *b)
#endif
{
        int k, wa, wb, wc;
        unsigned32 carry, y, z;
        unsigned32 *x, *xa, *xae, *xb, *xbe, *xc, *xc0;
        unsigned32 z2;
        if (a->wds < b->wds) {
                Bigint *tmp = a;
                a = b;
                b = tmp;
                }
        k = a->k;
        wa = a->wds;
        wb = b->wds;
        wc = wa + wb;
        if (wc > a->maxwds)
                k++;
	c = Brealloc(c, k);
        for(x = c->x, xa = x + wc; x < xa; x++)
                *x = 0;
        xa = a->x;
        xae = xa + wa;
        xb = b->x;
        xbe = xb + wb;
        xc0 = c->x;
        for(; xb < xbe; xb++, xc0++) {
                if ((y = *xb & 0xffff)) {
                        x = xa;
                        xc = xc0;
                        carry = 0;
                        do {
                                z = (*x & 0xffff) * y + (*xc & 0xffff) + carry;
                                carry = z >> 16;
                                z2 = (*x++ >> 16) * y + (*xc >> 16) + carry;
                                carry = z2 >> 16;
                                Storeinc(xc, z2, z);
                                }
                                while(x < xae);
                        *xc = carry;
                        }
                if ((y = *xb >> 16)) {
                        x = xa;
                        xc = xc0;
                        carry = 0;
                        z2 = *xc;
                        do {
                                z = (*x & 0xffff) * y + (*xc >> 16) + carry;
                                carry = z >> 16;
                                Storeinc(xc, z, z2);
                                z2 = (*x++ >> 16) * y + (*xc & 0xffff) + carry;
                                carry = z2 >> 16;
                                }
                                while(x < xae);
                        *xc = z2;
                        }
                }
        for(xc0 = c->x, xc = xc0 + wc; wc > 0 && !*--xc; --wc) ;
        c->wds = wc;
        return c;
        }

/* Returns b*(5**k).  b is modified. */
/* Re-written by Per Bothner to not need a static list. */

static Bigint *
pow5mult
#ifdef KR_headers
        (b, k) Bigint *b; int k;
#else
        (Bigint *b, int k)
#endif
{
  static int p05[6] = { 5, 25, 125, 625, 3125, 15625 };

  for (; k > 6; k -= 6)
    b = multadd(b, 15625, 0); /* b *= 5**6 */
  if (k == 0)
    return b;
  else
    return multadd(b, p05[k-1], 0);
}

/* Re-written by Per Bothner so shift can be in place. */

static Bigint *
lshift
#ifdef KR_headers
	(b, k) Bigint *b; int k;
#else
        (Bigint *b, int k)
#endif
{
  int i;
  unsigned32 *x, *x1, *xe;
  int old_wds = b->wds;
  int n = k >> 5;
  int k1 = b->k;
  int n1 = n + old_wds + 1;

  if (k == 0)
    return b;

  for(i = b->maxwds; n1 > i; i <<= 1)
    k1++;
  b = Brealloc(b, k1);

  xe = b->x; /* Source limit */
  x = xe + old_wds; /* Source pointer */
  x1 = x + n; /* Destination pointer */
  if (k &= 0x1f) {
    int k1 = 32 - k;
    unsigned32 z = *--x;
    if ((*x1 = (z >> k1)) != 0) {
      ++n1;
    }
    while (x > xe) {
      unsigned32 w = *--x;
      *--x1 = (z << k) | (w >> k1);
      z = w;
    }
    *--x1 = z << k;
  }
  else
    do {
      *--x1 = *--x;
    } while(x > xe);
  while (x1 > xe)
    *--x1 = 0;
  b->wds = n1 - 1;
  return b;
}

static int
cmp
#ifdef KR_headers
        (a, b) Bigint *a, *b;
#else
        (Bigint *a, Bigint *b)
#endif
{
        unsigned32 *xa, *xa0, *xb, *xb0;
        int i, j;

        i = a->wds;
        j = b->wds;
#ifdef DEBUG
        if (i > 1 && !a->x[i-1])
                Bug("cmp called with a->x[a->wds-1] == 0");
        if (j > 1 && !b->x[j-1])
                Bug("cmp called with b->x[b->wds-1] == 0");
#endif
        if (i -= j)
                return i;
        xa0 = a->x;
        xa = xa0 + j;
        xb0 = b->x;
        xb = xb0 + j;
        for(;;) {
                if (*--xa != *--xb)
                        return *xa < *xb ? -1 : 1;
                if (xa <= xa0)
                        break;
                }
        return 0;
        }

/* Do: c = a-b. */

static Bigint *
diff
#ifdef KR_headers
        (c, a, b) Bigint *c, *a, *b;
#else
        (Bigint *c, Bigint *a, Bigint *b)
#endif
{
        int i, wa, wb;
        _G_int32_t borrow, y; /* We need signed shifts here. */
        unsigned32 *xa, *xae, *xb, *xbe, *xc;
        _G_int32_t z;

        i = cmp(a,b);
        if (!i) {
                c = Brealloc(c, 0);
                c->wds = 1;
                c->x[0] = 0;
                return c;
                }
        if (i < 0) {
                Bigint *tmp = a;
                a = b;
                b = tmp;
                i = 1;
                }
        else
                i = 0;
        c = Brealloc(c, a->k);
        c->sign = i;
        wa = a->wds;
        xa = a->x;
        xae = xa + wa;
        wb = b->wds;
        xb = b->x;
        xbe = xb + wb;
        xc = c->x;
        borrow = 0;
        do {
                y = (*xa & 0xffff) - (*xb & 0xffff) + borrow;
                borrow = y >> 16;
                Sign_Extend(borrow, y);
                z = (*xa++ >> 16) - (*xb++ >> 16) + borrow;
                borrow = z >> 16;
                Sign_Extend(borrow, z);
                Storeinc(xc, z, y);
                }
                while(xb < xbe);
        while(xa < xae) {
                y = (*xa & 0xffff) + borrow;
                borrow = y >> 16;
                Sign_Extend(borrow, y);
                z = (*xa++ >> 16) + borrow;
                borrow = z >> 16;
                Sign_Extend(borrow, z);
                Storeinc(xc, z, y);
                }
        while(!*--xc)
                wa--;
        c->wds = wa;
        return c;
        }

static double
ulp
#ifdef KR_headers
        (x) double x;
#else
        (double x)
#endif
{
        register _G_int32_t L;
        double a;

        L = (word0(x) & Exp_mask) - (P-1)*Exp_msk1;
#ifndef Sudden_Underflow
        if (L > 0) {
#endif
#ifdef IBM
                L |= Exp_msk1 >> 4;
#endif
                setwords(a, L, 0);
#ifndef Sudden_Underflow
                }
        else {
                L = -L >> Exp_shift;
                if (L < Exp_shift)
                        setwords(a, 0x80000 >> L, 0);
                else {
                        L -= Exp_shift;
                        setwords(a, 0, L >= 31 ? 1 : 1 << (31 - L));
                        }
                }
#endif
        return a;
        }

static double
b2d
#ifdef KR_headers
        (a, e) Bigint *a; int *e;
#else
        (Bigint *a, int *e)
#endif
{
        unsigned32 *xa, *xa0, w, y, z;
        int k;
        double d;
        unsigned32 d0, d1;

        xa0 = a->x;
        xa = xa0 + a->wds;
        y = *--xa;
#ifdef DEBUG
        if (!y) Bug("zero y in b2d");
#endif
        k = hi0bits(y);
        *e = 32 - k;
        if (k < Ebits) {
                d0 = Exp_1 | y >> (Ebits - k);
                w = xa > xa0 ? *--xa : 0;
#ifndef _DOUBLE_IS_32BITS
                d1 = y << ((32-Ebits) + k) | w >> (Ebits - k);
#endif
                goto ret_d;
                }
        z = xa > xa0 ? *--xa : 0;
        if (k -= Ebits) {
                d0 = Exp_1 | y << k | z >> (32 - k);
                y = xa > xa0 ? *--xa : 0;
#ifndef _DOUBLE_IS_32BITS
                d1 = z << k | y >> (32 - k);
#endif
                }
        else {
                d0 = Exp_1 | y;
#ifndef _DOUBLE_IS_32BITS
                d1 = z;
#endif
                }
 ret_d:
#ifdef VAX
        setwords(d, d0 >> 16 | d0 << 16, d1 >> 16 | d1 << 16);
#else
	setwords (d, d0, d1);
#endif
        return d;
        }

static Bigint *
d2b
#ifdef KR_headers
        (result, d, e, bits) Bigint *result; double d; _G_int32_t *e, *bits;
#else
        (Bigint *result, double d, _G_int32_t *e, _G_int32_t *bits)
#endif
{
        int de, i, k;
        unsigned32 *x, y, z;
        unsigned32 d0, d1;
#ifdef VAX
        d0 = word0(d) >> 16 | word0(d) << 16;
        d1 = word1(d) >> 16 | word1(d) << 16;
#else
	d0 = word0(d);
	d1 = word1(d);
#endif

        result = Brealloc(result, 1);
        x = result->x;

        z = d0 & Frac_mask;
        d0 &= 0x7fffffff;       /* clear sign bit, which we ignore */

        de = (int)(d0 >> Exp_shift);  /* The exponent part of d. */

	/* Put back the suppressed high-order bit, if normalized. */
#ifndef IBM
#ifndef Sudden_Underflow
        if (de)
#endif
	  z |= Exp_msk11;
#endif

#ifndef _DOUBLE_IS_32BITS
        if ((y = d1)) {
                if ((k = lo0bits(&y))) {
                        x[0] = y | z << (32 - k);
                        z >>= k;
                        }
                else
                        x[0] = y;
                i = result->wds = (x[1] = z) ? 2 : 1;
                }
        else {
#endif /* !_DOUBLE_IS_32BITS */
#ifdef DEBUG
                if (!z)
                        Bug("Zero passed to d2b");
#endif
                k = lo0bits(&z);
                x[0] = z;
                i = result->wds = 1;
#ifndef _DOUBLE_IS_32BITS
                k += 32;
                }
#endif
#ifndef Sudden_Underflow
        if (de) {
#endif
#ifdef IBM
                *e = (de - Bias - (P-1) << 2) + k;
                *bits = 4*P + 8 - k - hi0bits(word0(d) & Frac_mask);
#else
                *e = de - Bias - (P-1) + k;
                *bits = P - k;
#endif
#ifndef Sudden_Underflow
                }
        else {
                *e = de - Bias - (P-1) + 1 + k;
                *bits = 32*i - hi0bits(x[i-1]);
                }
#endif
        return result;
        }

static double
ratio
#ifdef KR_headers
        (a, b) Bigint *a, *b;
#else
        (Bigint *a, Bigint *b)
#endif
{
        double da, db;
        int k, ka, kb;

        da = b2d(a, &ka);
        db = b2d(b, &kb);
        k = ka - kb + 32*(a->wds - b->wds);
#ifdef IBM
        if (k > 0) {
                addword0(da, (k >> 2)*Exp_msk1);
                if (k &= 3)
                        da *= 1 << k;
                }
        else {
                k = -k;
                addword0(db,(k >> 2)*Exp_msk1);
                if (k &= 3)
                        db *= 1 << k;
                }
#else
        if (k > 0)
                addword0(da, k*Exp_msk1);
        else {
                k = -k;
                addword0(db, k*Exp_msk1);
                }
#endif
        return da / db;
        }

static CONST double
tens[] = {
                1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
                1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
                1e20, 1e21, 1e22
#ifdef VAX
                , 1e23, 1e24
#endif
                };

#ifdef IEEE_Arith
static CONST double bigtens[] = { 1e16, 1e32, 1e64, 1e128, 1e256 };
static CONST double tinytens[] = { 1e-16, 1e-32, 1e-64, 1e-128, 1e-256 };
#define n_bigtens 5
#else
#ifdef IBM
static CONST double bigtens[] = { 1e16, 1e32, 1e64 };
static CONST double tinytens[] = { 1e-16, 1e-32, 1e-64 };
#define n_bigtens 3
#else
/* Also used for the case when !_DOUBLE_IS_32BITS.  */
static CONST double bigtens[] = { 1e16, 1e32 };
static CONST double tinytens[] = { 1e-16, 1e-32 };
#define n_bigtens 2
#endif
#endif

 double
_IO_strtod
#ifdef KR_headers
        (s00, se) CONST char *s00; char **se;
#else
        (CONST char *s00, char **se)
#endif
{
        _G_int32_t bb2, bb5, bbe, bd2, bd5, bbbits, bs2, c, dsign,
                 e, e1, esign, i, j, k, nd, nd0, nf, nz, nz0, sign;
        CONST char *s, *s0, *s1;
        double aadj, aadj1, adj, rv, rv0;
        _G_int32_t L;
        unsigned32 y, z;
	Bigint _bb, _b_avail, _bd, _bd0, _bs, _delta;
	Bigint *bb = Binit(&_bb);
	Bigint *bd = Binit(&_bd);
	Bigint *bd0 = Binit(&_bd0);
	Bigint *bs = Binit(&_bs);
	Bigint *b_avail = Binit(&_b_avail);
	Bigint *delta = Binit(&_delta);

	TEST_ENDIANNESS;
        sign = nz0 = nz = 0;
        rv = 0.;
	(void)&rv;		/* Force rv into the stack */
        for(s = s00;;s++) switch(*s) {
                case '-':
                        sign = 1;
                        /* no break */
                case '+':
                        if (*++s)
                                goto break2;
                        /* no break */
                case 0:
			/* "+" and "-" should be reported as an error? */
			sign = 0;
			s = s00;
                        goto ret;
                case '\t':
                case '\n':
                case '\v':
                case '\f':
                case '\r':
                case ' ':
                        continue;
                default:
                        goto break2;
                }
 break2:
        if (*s == '0') {
                nz0 = 1;
                while(*++s == '0') ;
                if (!*s)
                        goto ret;
                }
        s0 = s;
        y = z = 0;
        for(nd = nf = 0; (c = *s) >= '0' && c <= '9'; nd++, s++)
                if (nd < 9)
                        y = 10*y + c - '0';
                else if (nd < 16)
                        z = 10*z + c - '0';
        nd0 = nd;
        if (c == '.') {
                c = *++s;
                if (!nd) {
                        for(; c == '0'; c = *++s)
                                nz++;
                        if (c > '0' && c <= '9') {
                                s0 = s;
                                nf += nz;
                                nz = 0;
                                goto have_dig;
                                }
                        goto dig_done;
                        }
                for(; c >= '0' && c <= '9'; c = *++s) {
 have_dig:
                        nz++;
                        if (c -= '0') {
                                nf += nz;
                                for(i = 1; i < nz; i++)
                                        if (nd++ < 9)
                                                y *= 10;
                                        else if (nd <= DBL_DIG + 1)
                                                z *= 10;
                                if (nd++ < 9)
                                        y = 10*y + c;
                                else if (nd <= DBL_DIG + 1)
                                        z = 10*z + c;
                                nz = 0;
                                }
                        }
                }
 dig_done:
        e = 0;
        if (c == 'e' || c == 'E') {
                if (!nd && !nz && !nz0) {
                        s = s00;
                        goto ret;
                        }
                s00 = s;
                esign = 0;
                switch(c = *++s) {
                        case '-':
                                esign = 1;
                        case '+':
                                c = *++s;
                        }
                if (c >= '0' && c <= '9') {
                        while(c == '0')
                                c = *++s;
                        if (c > '0' && c <= '9') {
                                e = c - '0';
                                s1 = s;
                                while((c = *++s) >= '0' && c <= '9')
                                        e = 10*e + c - '0';
                                if (s - s1 > 8)
                                        /* Avoid confusion from exponents
                                         * so large that e might overflow.
                                         */
                                        e = 9999999;
                                if (esign)
                                        e = -e;
                                }
                        else
                                e = 0;
                        }
                else
                        s = s00;
                }
        if (!nd) {
                if (!nz && !nz0)
                        s = s00;
                goto ret;
                }
        e1 = e -= nf;

        /* Now we have nd0 digits, starting at s0, followed by a
         * decimal point, followed by nd-nd0 digits.  The number we're
         * after is the integer represented by those digits times
         * 10**e */

        if (!nd0)
                nd0 = nd;
        k = nd < DBL_DIG + 1 ? nd : DBL_DIG + 1;
        rv = y;
        if (k > 9)
                rv = tens[k - 9] * rv + z;
        if (nd <= DBL_DIG
#ifndef RND_PRODQUOT
                && FLT_ROUNDS == 1
#endif
                        ) {
                if (!e)
                        goto ret;
                if (e > 0) {
                        if (e <= Ten_pmax) {
#ifdef VAX
                                goto vax_ovfl_check;
#else
                                /* rv = */ rounded_product(rv, tens[e]);
                                goto ret;
#endif
                                }
                        i = DBL_DIG - nd;
                        if (e <= Ten_pmax + i) {
                                /* A fancier test would sometimes let us do
                                 * this for larger i values.
                                 */
                                e -= i;
                                rv *= tens[i];
#ifdef VAX
                                /* VAX exponent range is so narrow we must
                                 * worry about overflow here...
                                 */
 vax_ovfl_check:
                                addword0(rv, - P*Exp_msk1);
                                /* rv = */ rounded_product(rv, tens[e]);
                                if ((word0(rv) & Exp_mask)
                                 > Exp_msk1*(DBL_MAX_EXP+Bias-1-P))
                                        goto ovfl;
                                addword0(rv, P*Exp_msk1);
#else
                                /* rv = */ rounded_product(rv, tens[e]);
#endif
                                goto ret;
                                }
                        }
#ifndef Inaccurate_Divide
                else if (e >= -Ten_pmax) {
                        /* rv = */ rounded_quotient(rv, tens[-e]);
                        goto ret;
                        }
#endif
                }
        e1 += nd - k;

        /* Get starting approximation = rv * 10**e1 */

        if (e1 > 0) {
                if ((i = e1 & 15))
                        rv *= tens[i];
                if (e1 &= ~15) {
                        if (e1 > DBL_MAX_10_EXP) {
 ovfl:
                                errno = ERANGE;
#if defined(sun) && !defined(__svr4__)
/* SunOS defines HUGE_VAL as __infinity(), which is in libm. */
#undef HUGE_VAL
#endif
#ifndef HUGE_VAL
#define HUGE_VAL        1.7976931348623157E+308
#endif
                                rv = HUGE_VAL;
                                goto ret;
                                }
                        if (e1 >>= 4) {
                                for(j = 0; e1 > 1; j++, e1 >>= 1)
                                        if (e1 & 1)
                                                rv *= bigtens[j];
                        /* The last multiplication could overflow. */
                                addword0(rv, -P*Exp_msk1);
                                rv *= bigtens[j];
                                if ((z = word0(rv) & Exp_mask)
                                 > Exp_msk1*(DBL_MAX_EXP+Bias-P))
                                        goto ovfl;
                                if (z > Exp_msk1*(DBL_MAX_EXP+Bias-1-P)) {
                                        /* set to largest number */
                                        /* (Can't trust DBL_MAX) */
                                        setwords(rv, Big0, Big1);
                                        }
                                else
                                        addword0(rv, P*Exp_msk1);
                                }

                        }
                }
        else if (e1 < 0) {
                e1 = -e1;
                if ((i = e1 & 15))
                        rv /= tens[i];
                if (e1 &= ~15) {
                        e1 >>= 4;
                        for(j = 0; e1 > 1; j++, e1 >>= 1)
                                if (e1 & 1)
                                        rv *= tinytens[j];
                        /* The last multiplication could underflow. */
                        rv0 = rv;
                        rv *= tinytens[j];
                        if (!rv) {
                                rv = 2.*rv0;
                                rv *= tinytens[j];
                                if (!rv) {
 undfl:
                                        rv = 0.;
                                        errno = ERANGE;
                                        goto ret;
                                        }
                                setwords(rv, Tiny0, Tiny1);
                                /* The refinement below will clean
                                 * this approximation up.
                                 */
                                }
                        }
                }

        /* Now the hard part -- adjusting rv to the correct value.*/

        /* Put digits into bd: true value = bd * 10^e */

        bd0 = s2b(bd0, s0, nd0, nd, y);
	bd = Brealloc(bd, bd0->k);

        for(;;) {
                Bcopy(bd, bd0);
                bb = d2b(bb, rv, &bbe, &bbbits);    /* rv = bb * 2^bbe */
                bs = i2b(bs, 1);

                if (e >= 0) {
                        bb2 = bb5 = 0;
                        bd2 = bd5 = e;
                        }
                else {
                        bb2 = bb5 = -e;
                        bd2 = bd5 = 0;
                        }
                if (bbe >= 0)
                        bb2 += bbe;
                else
                        bd2 -= bbe;
                bs2 = bb2;
#ifdef Sudden_Underflow
#ifdef IBM
                j = 1 + 4*P - 3 - bbbits + ((bbe + bbbits - 1) & 3);
#else
                j = P + 1 - bbbits;
#endif
#else
                i = bbe + bbbits - 1;   /* logb(rv) */
                if (i < Emin)   /* denormal */
                        j = bbe + (P-Emin);
                else
                        j = P + 1 - bbbits;
#endif
                bb2 += j;
                bd2 += j;
                i = bb2 < bd2 ? bb2 : bd2;
                if (i > bs2)
                        i = bs2;
                if (i > 0) {
                        bb2 -= i;
                        bd2 -= i;
                        bs2 -= i;
                        }
                if (bb5 > 0) {
			Bigint *b_tmp;
                        bs = pow5mult(bs, bb5);
                        b_tmp = mult(b_avail, bs, bb);
                        b_avail = bb;
                        bb = b_tmp;
                        }
                if (bb2 > 0)
                        bb = lshift(bb, bb2);
                if (bd5 > 0)
                        bd = pow5mult(bd, bd5);
                if (bd2 > 0)
                        bd = lshift(bd, bd2);
                if (bs2 > 0)
                        bs = lshift(bs, bs2);
                delta = diff(delta, bb, bd);
                dsign = delta->sign;
                delta->sign = 0;
                i = cmp(delta, bs);
                if (i < 0) {
                        /* Error is less than half an ulp -- check for
                         * special case of mantissa a power of two.
                         */
                        if (dsign || word1(rv) || word0(rv) & Bndry_mask)
                                break;
                        delta = lshift(delta,Log2P);
                        if (cmp(delta, bs) > 0)
                                goto drop_down;
                        break;
                        }
                if (i == 0) {
                        /* exactly half-way between */
                        if (dsign) {
                                if ((word0(rv) & Bndry_mask1) == Bndry_mask1
                                 &&  word1(rv) == 0xffffffff) {
                                        /*boundary case -- increment exponent*/
                                        setword0(rv, (word0(rv) & Exp_mask)
						 + Exp_msk1);
#ifdef IBM
                                        setword0 (rv,
						  word0(rv) | (Exp_msk1 >> 4));
#endif
                                        setword1(rv, 0);
                                        break;
                                        }
                                }
                        else if (!(word0(rv) & Bndry_mask) && !word1(rv)) {
 drop_down:
                                /* boundary case -- decrement exponent */
#ifdef Sudden_Underflow
                                L = word0(rv) & Exp_mask;
#ifdef IBM
                                if (L <  Exp_msk1)
#else
                                if (L <= Exp_msk1)
#endif
                                        goto undfl;
                                L -= Exp_msk1;
#else
                                L = (word0(rv) & Exp_mask) - Exp_msk1;
#endif
                                setwords(rv, L | Bndry_mask1, 0xffffffff);
#ifdef IBM
                                continue;
#else
                                break;
#endif
                                }
#ifndef ROUND_BIASED
                        if (!(word1(rv) & LSB))
                                break;
#endif
                        if (dsign)
                                rv += ulp(rv);
#ifndef ROUND_BIASED
                        else {
                                rv -= ulp(rv);
#ifndef Sudden_Underflow
                                if (!rv)
                                        goto undfl;
#endif
                                }
#endif
                        break;
                        }
                if ((aadj = ratio(delta, bs)) <= 2.) {
                        if (dsign)
                                aadj = aadj1 = 1.;
                        else if (word1(rv) || word0(rv) & Bndry_mask) {
#ifndef Sudden_Underflow
                                if (word1(rv) == Tiny1 && !word0(rv))
                                        goto undfl;
#endif
                                aadj = 1.;
                                aadj1 = -1.;
                                }
                        else {
                                /* special case -- power of FLT_RADIX to be */
                                /* rounded down... */

                                if (aadj < 2./FLT_RADIX)
                                        aadj = 1./FLT_RADIX;
                                else
                                        aadj *= 0.5;
                                aadj1 = -aadj;
                                }
                        }
                else {
                        aadj *= 0.5;
                        aadj1 = dsign ? aadj : -aadj;
#ifdef Check_FLT_ROUNDS
                        switch(FLT_ROUNDS) {
                                case 2: /* towards +infinity */
                                        aadj1 -= 0.5;
                                        break;
                                case 0: /* towards 0 */
                                case 3: /* towards -infinity */
                                        aadj1 += 0.5;
                                }
#else
                        if (FLT_ROUNDS == 0)
                                aadj1 += 0.5;
#endif
                        }
                y = word0(rv) & Exp_mask;

                /* Check for overflow */

                if (y == Exp_msk1*(DBL_MAX_EXP+Bias-1)) {
                        rv0 = rv;
                        addword0(rv, - P*Exp_msk1);
                        adj = aadj1 * ulp(rv);
                        rv += adj;
                        if ((word0(rv) & Exp_mask) >=
                                        Exp_msk1*(DBL_MAX_EXP+Bias-P)) {
                                if (word0(rv0) == Big0 && word1(rv0) == Big1)
                                        goto ovfl;
                                setwords(rv, Big0, Big1);
                                continue;
                                }
                        else
                                addword0(rv, P*Exp_msk1);
                        }
                else {
#ifdef Sudden_Underflow
                        if ((word0(rv) & Exp_mask) <= P*Exp_msk1) {
                                rv0 = rv;
                                addword0(rv, P*Exp_msk1);
                                adj = aadj1 * ulp(rv);
                                rv += adj;
#ifdef IBM
                                if ((word0(rv) & Exp_mask) <  P*Exp_msk1)
#else
                                if ((word0(rv) & Exp_mask) <= P*Exp_msk1)
#endif
                                        {
                                        if (word0(rv0) == Tiny0
                                         && word1(rv0) == Tiny1)
                                                goto undfl;
                                        setwords(rv, Tiny0, Tiny1);
                                        continue;
                                        }
                                else
                                        addword0(rv, -P*Exp_msk1);
                                }
                        else {
                                adj = aadj1 * ulp(rv);
                                rv += adj;
                                }
#else
                        /* Compute adj so that the IEEE rounding rules will
                         * correctly round rv + adj in some half-way cases.
                         * If rv * ulp(rv) is denormalized (i.e.,
                         * y <= (P-1)*Exp_msk1), we must adjust aadj to avoid
                         * trouble from bits lost to denormalization;
                         * example: 1.2e-307 .
                         */
                        if (y <= (P-1)*Exp_msk1 && aadj >= 1.) {
                                aadj1 = (double)(int)(aadj + 0.5);
                                if (!dsign)
                                        aadj1 = -aadj1;
                                }
                        adj = aadj1 * ulp(rv);
                        rv += adj;
#endif
                        }
                z = word0(rv) & Exp_mask;
                if (y == z) {
                        /* Can we stop now? */
                        L = (_G_int32_t)aadj;
                        aadj -= L;
                        /* The tolerances below are conservative. */
                        if (dsign || word1(rv) || word0(rv) & Bndry_mask) {
                                if (aadj < .4999999 || aadj > .5000001)
                                        break;
                                }
                        else if (aadj < .4999999/FLT_RADIX)
                                break;
                        }
                }
        Bfree(bb);
        Bfree(bd);
        Bfree(bs);
        Bfree(bd0);
        Bfree(delta);
	Bfree(b_avail);
 ret:
        if (se)
                *se = (char *)s;
        return sign ? -rv : rv;
        }

static int
quorem
#ifdef KR_headers
        (b, S) Bigint *b, *S;
#else
        (Bigint *b, Bigint *S)
#endif
{
        int n;
        _G_int32_t borrow, y;
        unsigned32 carry, q, ys;
        unsigned32 *bx, *bxe, *sx, *sxe;
        _G_int32_t z;
        unsigned32 si, zs;

        n = S->wds;
#ifdef DEBUG
        /*debug*/ if (b->wds > n)
        /*debug*/       Bug("oversize b in quorem");
#endif
        if (b->wds < n)
                return 0;
        sx = S->x;
        sxe = sx + --n;
        bx = b->x;
        bxe = bx + n;
        q = *bxe / (*sxe + 1);  /* ensure q <= true quotient */
#ifdef DEBUG
        /*debug*/ if (q > 9)
        /*debug*/       Bug("oversized quotient in quorem");
#endif
        if (q) {
                borrow = 0;
                carry = 0;
                do {
                        si = *sx++;
                        ys = (si & 0xffff) * q + carry;
                        zs = (si >> 16) * q + (ys >> 16);
                        carry = zs >> 16;
                        y = (*bx & 0xffff) - (ys & 0xffff) + borrow;
                        borrow = y >> 16;
                        Sign_Extend(borrow, y);
                        z = (*bx >> 16) - (zs & 0xffff) + borrow;
                        borrow = z >> 16;
                        Sign_Extend(borrow, z);
                        Storeinc(bx, z, y);
                        }
                        while(sx <= sxe);
                if (!*bxe) {
                        bx = b->x;
                        while(--bxe > bx && !*bxe)
                                --n;
                        b->wds = n;
                        }
                }
        if (cmp(b, S) >= 0) {
                q++;
                borrow = 0;
                carry = 0;
                bx = b->x;
                sx = S->x;
                do {
                        si = *sx++;
                        ys = (si & 0xffff) + carry;
                        zs = (si >> 16) + (ys >> 16);
                        carry = zs >> 16;
                        y = (*bx & 0xffff) - (ys & 0xffff) + borrow;
                        borrow = y >> 16;
                        Sign_Extend(borrow, y);
                        z = (*bx >> 16) - (zs & 0xffff) + borrow;
                        borrow = z >> 16;
                        Sign_Extend(borrow, z);
                        Storeinc(bx, z, y);
                        }
                        while(sx <= sxe);
                bx = b->x;
                bxe = bx + n;
                if (!*bxe) {
                        while(--bxe > bx && !*bxe)
                                --n;
                        b->wds = n;
                        }
                }
        return q;
        }

/* dtoa for IEEE arithmetic (dmg): convert double to ASCII string.
 *
 * Inspired by "How to Print Floating-Point Numbers Accurately" by
 * Guy L. Steele, Jr. and Jon L. White [Proc. ACM SIGPLAN '90, pp. 92-101].
 *
 * Modifications:
 *      1. Rather than iterating, we use a simple numeric overestimate
 *         to determine k = floor(log10(d)).  We scale relevant
 *         quantities using O(log2(k)) rather than O(k) multiplications.
 *      2. For some modes > 2 (corresponding to ecvt and fcvt), we don't
 *         try to generate digits strictly left to right.  Instead, we
 *         compute with fewer bits and propagate the carry if necessary
 *         when rounding the final digit up.  This is often faster.
 *      3. Under the assumption that input will be rounded nearest,
 *         mode 0 renders 1e23 as 1e23 rather than 9.999999999999999e22.
 *         That is, we allow equality in stopping tests when the
 *         round-nearest rule will give the same floating-point value
 *         as would satisfaction of the stopping test with strict
 *         inequality.
 *      4. We remove common factors of powers of 2 from relevant
 *         quantities.
 *      5. When converting floating-point integers less than 1e16,
 *         we use floating-point arithmetic rather than resorting
 *         to multiple-precision integers.
 *      6. When asked to produce fewer than 15 digits, we first try
 *         to get by with floating-point arithmetic; we resort to
 *         multiple-precision integer arithmetic only if we cannot
 *         guarantee that the floating-point calculation has given
 *         the correctly rounded result.  For k requested digits and
 *         "uniformly" distributed input, the probability is
 *         something like 10^(k-15) that we must resort to the long
 *         calculation.
 */

 char *
_IO_dtoa
#ifdef KR_headers
        (d, mode, ndigits, decpt, sign, rve)
        double d; int mode, ndigits, *decpt, *sign; char **rve;
#else
        (double d, int mode, int ndigits, int *decpt, int *sign, char **rve)
#endif
{
 /*     Arguments ndigits, decpt, sign are similar to those
        of ecvt and fcvt; trailing zeros are suppressed from
        the returned string.  If not null, *rve is set to point
        to the end of the return value.  If d is +-Infinity or NaN,
        then *decpt is set to 9999.

        mode:
                0 ==> shortest string that yields d when read in
                        and rounded to nearest.
                1 ==> like 0, but with Steele & White stopping rule;
                        e.g. with IEEE P754 arithmetic , mode 0 gives
                        1e23 whereas mode 1 gives 9.999999999999999e22.
                2 ==> max(1,ndigits) significant digits.  This gives a
                        return value similar to that of ecvt, except
                        that trailing zeros are suppressed.
                3 ==> through ndigits past the decimal point.  This
                        gives a return value similar to that from fcvt,
                        except that trailing zeros are suppressed, and
                        ndigits can be negative.
                4-9 should give the same return values as 2-3, i.e.,
                        4 <= mode <= 9 ==> same return as mode
                        2 + (mode & 1).  These modes are mainly for
                        debugging; often they run slower but sometimes
                        faster than modes 2-3.
                4,5,8,9 ==> left-to-right digit generation.
                6-9 ==> don't try fast floating-point estimate
                        (if applicable).

                Values of mode other than 0-9 are treated as mode 0.

                Sufficient space is allocated to the return value
                to hold the suppressed trailing zeros.
        */

        _G_int32_t bbits, b2, b5, be, dig, i, ieps, ilim, ilim0, ilim1,
                j, j1, k, k0, k_check, leftright, m2, m5, s2, s5,
                spec_case, try_quick;
        _G_int32_t L;
#ifndef Sudden_Underflow
        int denorm;
#endif
	Bigint _b_avail, _b, _mhi, _mlo, _S;
	Bigint *b_avail = Binit(&_b_avail);
	Bigint *b = Binit(&_b);
	Bigint *S = Binit(&_S);
	/* mhi and mlo are only set and used if leftright. */
        Bigint *mhi = NULL, *mlo = NULL;
        double d2, ds, eps;
        char *s, *s0;
        static Bigint *result = NULL;
        static int result_k;

	TEST_ENDIANNESS;
        if (result) {
		/* result is contains a string, so its fields (interpreted
		   as a Bigint have been trashed.  Restore them.
		   This is a really ugly interface - result should
		   not be static, since that is not thread-safe.  FIXME. */
                result->k = result_k;
                result->maxwds = 1 << result_k;
                result->on_stack = 0;
                }

        if (word0(d) & Sign_bit) {
                /* set sign for everything, including 0's and NaNs */
                *sign = 1;
                setword0(d, word0(d) & ~Sign_bit);  /* clear sign bit */
                }
        else
                *sign = 0;

#if defined(IEEE_Arith) + defined(VAX)
#ifdef IEEE_Arith
        if ((word0(d) & Exp_mask) == Exp_mask)
#else
        if (word0(d)  == 0x8000)
#endif
                {
                /* Infinity or NaN */
                *decpt = 9999;
#ifdef IEEE_Arith
		if (!word1(d) && !(word0(d) & 0xfffff))
		  {
		    s = "Infinity";
		    if (rve)
		      *rve = s + 8;
		  }
		else
#endif
		  {
		    s = "NaN";
		    if (rve)
		      *rve = s +3;
		  }
                return s;
                }
#endif
#ifdef IBM
        d += 0; /* normalize */
#endif
        if (!d) {
                *decpt = 1;
                s = "0";
                if (rve)
                        *rve = s + 1;
                return s;
                }

        b = d2b(b, d, &be, &bbits);
        i = (int)(word0(d) >> Exp_shift1 & (Exp_mask>>Exp_shift1));
#ifndef Sudden_Underflow
        if (i) {
#endif
                d2 = d;
                setword0(d2, (word0(d2) & Frac_mask1) | Exp_11);
#ifdef IBM
                if (j = 11 - hi0bits(word0(d2) & Frac_mask))
                        d2 /= 1 << j;
#endif

                i -= Bias;
#ifdef IBM
                i <<= 2;
                i += j;
#endif
#ifndef Sudden_Underflow
                denorm = 0;
                }
        else {
                /* d is denormalized */
		unsigned32 x;

                i = bbits + be + (Bias + (P-1) - 1);
                x = i > 32  ? word0(d) << (64 - i) | word1(d) >> (i - 32)
                            : word1(d) << (32 - i);
                d2 = x;
                addword0(d2, - 31*Exp_msk1); /* adjust exponent */
                i -= (Bias + (P-1) - 1) + 1;
                denorm = 1;
                }
#endif

	/* Now i is the unbiased base-2 exponent. */

        /* log(x)       ~=~ log(1.5) + (x-1.5)/1.5
         * log10(x)      =  log(x) / log(10)
         *              ~=~ log(1.5)/log(10) + (x-1.5)/(1.5*log(10))
         * log10(d) = i*log(2)/log(10) + log10(d2)
         *
         * This suggests computing an approximation k to log10(d) by
         *
         * k = i*0.301029995663981
         *      + ( (d2-1.5)*0.289529654602168 + 0.176091259055681 );
         *
         * We want k to be too large rather than too small.
         * The error in the first-order Taylor series approximation
         * is in our favor, so we just round up the constant enough
         * to compensate for any error in the multiplication of
         * (i) by 0.301029995663981; since |i| <= 1077,
         * and 1077 * 0.30103 * 2^-52 ~=~ 7.2e-14,
         * adding 1e-13 to the constant term more than suffices.
         * Hence we adjust the constant term to 0.1760912590558.
         * (We could get a more accurate k by invoking log10,
         *  but this is probably not worthwhile.)
         */

        ds = (d2-1.5)*0.289529654602168 + 0.1760912590558 + i*0.301029995663981;
        k = (int)ds;
        if (ds < 0. && ds != k)
                k--;    /* want k = floor(ds) */
        k_check = 1;
        if (k >= 0 && k <= Ten_pmax) {
                if (d < tens[k])
                        k--;
                k_check = 0;
                }
        j = bbits - i - 1;
        if (j >= 0) {
                b2 = 0;
                s2 = j;
                }
        else {
                b2 = -j;
                s2 = 0;
                }
        if (k >= 0) {
                b5 = 0;
                s5 = k;
                s2 += k;
                }
        else {
                b2 -= k;
                b5 = -k;
                s5 = 0;
                }
        if (mode < 0 || mode > 9)
                mode = 0;
        try_quick = 1;
        if (mode > 5) {
                mode -= 4;
                try_quick = 0;
                }
        leftright = 1;
        switch(mode) {
                case 0:
                case 1:
                        ilim = ilim1 = -1;
                        i = 18;
                        ndigits = 0;
                        break;
                case 2:
                        leftright = 0;
                        /* no break */
                case 4:
                        if (ndigits <= 0)
                                ndigits = 1;
                        ilim = ilim1 = i = ndigits;
                        break;
                case 3:
                        leftright = 0;
                        /* no break */
                case 5:
                        i = ndigits + k + 1;
                        ilim = i;
                        ilim1 = i - 1;
                        if (i <= 0)
                                i = 1;
                }
	/* i is now an upper bound of the number of digits to generate. */
        j = sizeof(unsigned32) * (1<<BIGINT_MINIMUM_K);
	/* The test is <= so as to allow room for the final '\0'. */
        for(result_k = BIGINT_MINIMUM_K; BIGINT_HEADER_SIZE + j <= i;
                j <<= 1) result_k++;
        if (!result || result_k > result->k)
        {
          Bfree (result);
          result = Balloc(result_k);
        }
        s = s0 = (char *)result;

        if (ilim >= 0 && ilim <= Quick_max && try_quick) {

                /* Try to get by with floating-point arithmetic. */

                i = 0;
                d2 = d;
                k0 = k;
                ilim0 = ilim;
                ieps = 2; /* conservative */
                if (k > 0) {
                        ds = tens[k&0xf];
                        j = k >> 4;
                        if (j & Bletch) {
                                /* prevent overflows */
                                j &= Bletch - 1;
                                d /= bigtens[n_bigtens-1];
                                ieps++;
                                }
                        for(; j; j >>= 1, i++)
                                if (j & 1) {
                                        ieps++;
                                        ds *= bigtens[i];
                                        }
                        d /= ds;
                        }
                else if ((j1 = -k)) {
                        d *= tens[j1 & 0xf];
                        for(j = j1 >> 4; j; j >>= 1, i++)
                                if (j & 1) {
                                        ieps++;
                                        d *= bigtens[i];
                                        }
                        }
                if (k_check && d < 1. && ilim > 0) {
                        if (ilim1 <= 0)
                                goto fast_failed;
                        ilim = ilim1;
                        k--;
                        d *= 10.;
                        ieps++;
                        }
                eps = ieps*d + 7.;
                addword0(eps, - (P-1)*Exp_msk1);
                if (ilim == 0) {
                        d -= 5.;
                        if (d > eps)
                                goto one_digit;
                        if (d < -eps)
                                goto no_digits;
                        goto fast_failed;
                        }
#ifndef No_leftright
                if (leftright) {
                        /* Use Steele & White method of only
                         * generating digits needed.
                         */
                        eps = 0.5/tens[ilim-1] - eps;
                        for(i = 0;;) {
                                L = (_G_int32_t)d;
                                d -= L;
                                *s++ = '0' + (int)L;
                                if (d < eps)
                                        goto ret1;
                                if (1. - d < eps)
                                        goto bump_up;
                                if (++i >= ilim)
                                        break;
                                eps *= 10.;
                                d *= 10.;
                                }
                        }
                else {
#endif
                        /* Generate ilim digits, then fix them up. */
                        eps *= tens[ilim-1];
                        for(i = 1;; i++, d *= 10.) {
                                L = (_G_int32_t)d;
                                d -= L;
                                *s++ = '0' + (int)L;
                                if (i == ilim) {
                                        if (d > 0.5 + eps)
                                                goto bump_up;
                                        else if (d < 0.5 - eps) {
                                                while(*--s == '0');
                                                s++;
                                                goto ret1;
                                                }
                                        break;
                                        }
                                }
#ifndef No_leftright
                        }
#endif
 fast_failed:
                s = s0;
                d = d2;
                k = k0;
                ilim = ilim0;
                }

        /* Do we have a "small" integer? */

        if (be >= 0 && k <= Int_max) {
                /* Yes. */
                ds = tens[k];
                if (ndigits < 0 && ilim <= 0) {
                        if (ilim < 0 || d <= 5*ds)
                                goto no_digits;
                        goto one_digit;
                        }
                for(i = 1;; i++) {
                        L = (_G_int32_t)(d / ds);
                        d -= L*ds;
#ifdef Check_FLT_ROUNDS
                        /* If FLT_ROUNDS == 2, L will usually be high by 1 */
                        if (d < 0) {
                                L--;
                                d += ds;
                                }
#endif
                        *s++ = '0' + (int)L;
                        if (i == ilim) {
                                d += d;
                                if (d > ds || (d == ds && L & 1)) {
 bump_up:
                                        while(*--s == '9')
                                                if (s == s0) {
                                                        k++;
                                                        *s = '0';
                                                        break;
                                                        }
                                        ++*s++;
                                        }
                                break;
                                }
                        if (!(d *= 10.))
                                break;
                        }
                goto ret1;
                }

        m2 = b2;
        m5 = b5;
        if (leftright) {
                if (mode < 2) {
                        i =
#ifndef Sudden_Underflow
                                denorm ? be + (Bias + (P-1) - 1 + 1) :
#endif
#ifdef IBM
                                1 + 4*P - 3 - bbits + ((bbits + be - 1) & 3);
#else
                                1 + P - bbits;
#endif
                        }
                else {
                        j = ilim - 1;
                        if (m5 >= j)
                                m5 -= j;
                        else {
                                s5 += j -= m5;
                                b5 += j;
                                m5 = 0;
                                }
                        if ((i = ilim) < 0) {
                                m2 -= i;
                                i = 0;
                                }
                        }
                b2 += i;
                s2 += i;
                mhi = i2b(Binit(&_mhi), 1);
                }
        if (m2 > 0 && s2 > 0) {
                i = m2 < s2 ? m2 : s2;
                b2 -= i;
                m2 -= i;
                s2 -= i;
                }
        if (b5 > 0) {
                if (leftright) {
                        if (m5 > 0) {
				Bigint *b_tmp;
                                mhi = pow5mult(mhi, m5);
                                b_tmp = mult(b_avail, mhi, b);
                                b_avail = b;
                                b = b_tmp;
                                }
                        if ((j = b5 - m5))
                                b = pow5mult(b, j);
                        }
                else
                        b = pow5mult(b, b5);
                }
        S = i2b(S, 1);
        if (s5 > 0)
                S = pow5mult(S, s5);

        /* Check for special case that d is a normalized power of 2. */

        if (mode < 2) {
                if (!word1(d) && !(word0(d) & Bndry_mask)
#ifndef Sudden_Underflow
                 && word0(d) & Exp_mask
#endif
                                ) {
                        /* The special case */
                        b2 += Log2P;
                        s2 += Log2P;
                        spec_case = 1;
                        }
                else
                        spec_case = 0;
                }

        /* Arrange for convenient computation of quotients:
         * shift left if necessary so divisor has 4 leading 0 bits.
         *
         * Perhaps we should just compute leading 28 bits of S once
         * and for all and pass them and a shift to quorem, so it
         * can do shifts and ors to compute the numerator for q.
         */
        if ((i = ((s5 ? 32 - hi0bits(S->x[S->wds-1]) : 1) + s2) & 0x1f))
                i = 32 - i;
        if (i > 4) {
                i -= 4;
                b2 += i;
                m2 += i;
                s2 += i;
                }
        else if (i < 4) {
                i += 28;
                b2 += i;
                m2 += i;
                s2 += i;
                }
        if (b2 > 0)
                b = lshift(b, b2);
        if (s2 > 0)
                S = lshift(S, s2);
        if (k_check) {
                if (cmp(b,S) < 0) {
                        k--;
                        b = multadd(b, 10, 0);  /* we botched the k estimate */
                        if (leftright)
                                mhi = multadd(mhi, 10, 0);
                        ilim = ilim1;
                        }
                }
        if (ilim <= 0 && mode > 2) {
                if (ilim < 0 || cmp(b,S = multadd(S,5,0)) <= 0) {
                        /* no digits, fcvt style */
 no_digits:
                        k = -1 - ndigits;
                        goto ret;
                        }
 one_digit:
                *s++ = '1';
                k++;
                goto ret;
                }
        if (leftright) {
                if (m2 > 0)
                        mhi = lshift(mhi, m2);

                /* Compute mlo -- check for special case
                 * that d is a normalized power of 2.
                 */

                if (spec_case) {
			mlo = Brealloc(Binit(&_mlo), mhi->k);
                        Bcopy(mlo, mhi);
                        mhi = lshift(mhi, Log2P);
                        }
		else
			mlo = mhi;

                for(i = 1;;i++) {
                        dig = quorem(b,S) + '0';
                        /* Do we yet have the shortest decimal string
                         * that will round to d?
                         */
                        j = cmp(b, mlo);
                        b_avail = diff(b_avail, S, mhi); /* b_avail = S - mi */
                        j1 = b_avail->sign ? 1 : cmp(b, b_avail);
#ifndef ROUND_BIASED
                        if (j1 == 0 && !mode && !(word1(d) & 1)) {
                                if (dig == '9')
                                        goto round_9_up;
                                if (j > 0)
                                        dig++;
                                *s++ = dig;
                                goto ret;
                                }
#endif
                        if (j < 0 || (j == 0 && !mode
#ifndef ROUND_BIASED
                                                        && !(word1(d) & 1)
#endif
                                        )) {
                                if (j1 > 0) {
                                        b = lshift(b, 1);
                                        j1 = cmp(b, S);
                                        if ((j1 > 0 || (j1 == 0 && dig & 1))
                                        && dig++ == '9')
                                                goto round_9_up;
                                        }
                                *s++ = dig;
                                goto ret;
                                }
                        if (j1 > 0) {
                                if (dig == '9') { /* possible if i == 1 */
 round_9_up:
                                        *s++ = '9';
                                        goto roundoff;
                                        }
                                *s++ = dig + 1;
                                goto ret;
                                }
                        *s++ = dig;
                        if (i == ilim)
                                break;
                        b = multadd(b, 10, 0);
                        if (mlo == mhi)
                                mlo = mhi = multadd(mhi, 10, 0);
                        else {
                                mlo = multadd(mlo, 10, 0);
                                mhi = multadd(mhi, 10, 0);
                                }
                        }
                }
        else
                for(i = 1;; i++) {
                        *s++ = dig = quorem(b,S) + '0';
                        if (i >= ilim)
                                break;
                        b = multadd(b, 10, 0);
                        }

        /* Round off last digit */

        b = lshift(b, 1);
        j = cmp(b, S);
        if (j > 0 || (j == 0 && dig & 1)) {
 roundoff:
                while(*--s == '9')
                        if (s == s0) {
                                k++;
                                *s++ = '1';
                                goto ret;
                                }
                ++*s++;
                }
        else {
                while(*--s == '0');
                s++;
                }
 ret:
	Bfree(b_avail);
        Bfree(S);
        if (mhi) {
                if (mlo && mlo != mhi)
                        Bfree(mlo);
                Bfree(mhi);
                }
 ret1:
        Bfree(b);
        *s = 0;
        *decpt = k + 1;
        if (rve)
                *rve = s;
        return s0;
        }
#endif /* _IO_USE_DTOA */
