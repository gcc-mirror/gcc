/*
FUNCTION
        <<strtod>>, <<strtodf>>---string to double or float

INDEX
	strtod
INDEX
	_strtod_r
INDEX
	strtodf

ANSI_SYNOPSIS
        #include <stdlib.h>
        double strtod(const char *<[str]>, char **<[tail]>);
        float strtodf(const char *<[str]>, char **<[tail]>);

        double _strtod_r(void *<[reent]>, 
                         const char *<[str]>, char **<[tail]>);

TRAD_SYNOPSIS
        #include <stdlib.h>
        double strtod(<[str]>,<[tail]>)
        char *<[str]>;
        char **<[tail]>;

        float strtodf(<[str]>,<[tail]>)
        char *<[str]>;
        char **<[tail]>;

        double _strtod_r(<[reent]>,<[str]>,<[tail]>)
	char *<[reent]>;
        char *<[str]>;
        char **<[tail]>;

DESCRIPTION
	The function <<strtod>> parses the character string <[str]>,
	producing a substring which can be converted to a double
	value.  The substring converted is the longest initial
	subsequence of <[str]>, beginning with the first
	non-whitespace character, that has the format:
	.[+|-]<[digits]>[.][<[digits]>][(e|E)[+|-]<[digits]>] 
	The substring contains no characters if <[str]> is empty, consists
	entirely of whitespace, or if the first non-whitespace
	character is something other than <<+>>, <<->>, <<.>>, or a
	digit. If the substring is empty, no conversion is done, and
	the value of <[str]> is stored in <<*<[tail]>>>.  Otherwise,
	the substring is converted, and a pointer to the final string
	(which will contain at least the terminating null character of
	<[str]>) is stored in <<*<[tail]>>>.  If you want no
	assignment to <<*<[tail]>>>, pass a null pointer as <[tail]>.
	<<strtodf>> is identical to <<strtod>> except for its return type.

	This implementation returns the nearest machine number to the
	input decimal string.  Ties are broken by using the IEEE
	round-even rule.

	The alternate function <<_strtod_r>> is a reentrant version.
	The extra argument <[reent]> is a pointer to a reentrancy structure.

RETURNS
	<<strtod>> returns the converted substring value, if any.  If
	no conversion could be performed, 0 is returned.  If the
	correct value is out of the range of representable values,
	plus or minus <<HUGE_VAL>> is returned, and <<ERANGE>> is
	stored in errno. If the correct value would cause underflow, 0
	is returned and <<ERANGE>> is stored in errno.

Supporting OS subroutines required: <<close>>, <<fstat>>, <<isatty>>,
<<lseek>>, <<read>>, <<sbrk>>, <<write>>.
*/

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

/* Please send bug reports to
	David M. Gay
	AT&T Bell Laboratories, Room 2C-463
	600 Mountain Avenue
	Murray Hill, NJ 07974-2070
	U.S.A.
	dmg@research.att.com or research!dmg
 */

#include <string.h>
#include <float.h>
#include <errno.h>
#include "mprec.h"

double
_DEFUN (_strtod_r, (ptr, s00, se),
	struct _Jv_reent *ptr _AND
	_CONST char *s00 _AND
	char **se)
{
  int bb2, bb5, bbe, bd2, bd5, bbbits, bs2, c, dsign, e1, esign, i, j,
    k, nd, nd0, nf, nz, nz0, sign;
  int digits = 0;  /* Number of digits found in fraction part. */
  long e;
  _CONST char *s, *s0, *s1;
  double aadj, aadj1, adj;
  long L;
  unsigned long y, z;
  union double_union rv, rv0;

  _Jv_Bigint *bb = NULL, *bb1, *bd = NULL, *bd0, *bs = NULL, *delta = NULL;
  sign = nz0 = nz = 0;
  rv.d = 0.;
  for (s = s00;; s++)
    switch (*s)
      {
      case '-':
	sign = 1;
	/* no break */
      case '+':
	if (*++s)
	  goto break2;
	/* no break */
      case 0:
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
  if (*s == '0')
    {
      digits++;
      nz0 = 1;
      while (*++s == '0')
	digits++;
      if (!*s)
	goto ret;
    }
  s0 = s;
  y = z = 0;
  for (nd = nf = 0; (c = *s) >= '0' && c <= '9'; nd++, s++)
    {
      digits++;
      if (nd < 9)
	y = 10 * y + c - '0';
      else if (nd < 16)
	z = 10 * z + c - '0';
    }
  nd0 = nd;
  if (c == '.')
    {
      c = *++s;
      if (!nd)
	{
	  for (; c == '0'; c = *++s)
	    {
	      digits++;
	      nz++;
	    }
	  if (c > '0' && c <= '9')
	    {
	      digits++;
	      s0 = s;
	      nf += nz;
	      nz = 0;
	      goto have_dig;
	    }
	  goto dig_done;
	}
      for (; c >= '0' && c <= '9'; c = *++s)
	{
	  digits++;
	have_dig:
	  nz++;
	  if (c -= '0')
	    {
	      nf += nz;
	      for (i = 1; i < nz; i++)
		if (nd++ < 9)
		  y *= 10;
		else if (nd <= DBL_DIG + 1)
		  z *= 10;
	      if (nd++ < 9)
		y = 10 * y + c;
	      else if (nd <= DBL_DIG + 1)
		z = 10 * z + c;
	      nz = 0;
	    }
	}
    }
dig_done:
  e = 0;
  if (c == 'e' || c == 'E')
    {
      if (!nd && !nz && !nz0)
	{
	  s = s00;
	  goto ret;
	}
      s00 = s;
      esign = 0;
      switch (c = *++s)
	{
	case '-':
	  esign = 1;
	case '+':
	  c = *++s;
	}
      if (c >= '0' && c <= '9')
	{
	  while (c == '0')
	    c = *++s;
	  if (c > '0' && c <= '9')
	    {
	      e = c - '0';
	      s1 = s;
	      while ((c = *++s) >= '0' && c <= '9')
		e = 10 * e + c - '0';
	      if (s - s1 > 8)
		/* Avoid confusion from exponents
		 * so large that e might overflow.
		 */
		e = 9999999L;
	      if (esign)
		e = -e;
	    }
	}
      else
	{
	  /* No exponent after an 'E' : that's an error. */
	  ptr->_errno = EINVAL;
	  e = 0;
	  s = s00;
	  goto ret;
	}
    }
  if (!nd)
    {
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
  rv.d = y;
  if (k > 9)
    rv.d = tens[k - 9] * rv.d + z;
  bd0 = 0;
  if (nd <= DBL_DIG
#ifndef RND_PRODQUOT
      && FLT_ROUNDS == 1
#endif
    )
    {
      if (!e)
	goto ret;
      if (e > 0)
	{
	  if (e <= Ten_pmax)
	    {
#ifdef VAX
	      goto vax_ovfl_check;
#else
	      /* rv.d = */ rounded_product (rv.d, tens[e]);
	      goto ret;
#endif
	    }
	  i = DBL_DIG - nd;
	  if (e <= Ten_pmax + i)
	    {
	      /* A fancier test would sometimes let us do
				 * this for larger i values.
				 */
	      e -= i;
	      rv.d *= tens[i];
#ifdef VAX
	      /* VAX exponent range is so narrow we must
	       * worry about overflow here...
	       */
	    vax_ovfl_check:
	      word0 (rv) -= P * Exp_msk1;
	      /* rv.d = */ rounded_product (rv.d, tens[e]);
	      if ((word0 (rv) & Exp_mask)
		  > Exp_msk1 * (DBL_MAX_EXP + Bias - 1 - P))
		goto ovfl;
	      word0 (rv) += P * Exp_msk1;
#else
	      /* rv.d = */ rounded_product (rv.d, tens[e]);
#endif
	      goto ret;
	    }
	}
#ifndef Inaccurate_Divide
      else if (e >= -Ten_pmax)
	{
	  /* rv.d = */ rounded_quotient (rv.d, tens[-e]);
	  goto ret;
	}
#endif
    }
  e1 += nd - k;

  /* Get starting approximation = rv.d * 10**e1 */

  if (e1 > 0)
    {
      if ((i = e1 & 15))
	rv.d *= tens[i];

      if (e1 &= ~15)
	{
	  if (e1 > DBL_MAX_10_EXP)
	    {
	    ovfl:
	      ptr->_errno = ERANGE;

	      /* Force result to IEEE infinity. */
	      word0 (rv) = Exp_mask;
	      word1 (rv) = 0;

	      if (bd0)
		goto retfree;
	      goto ret;
	    }
	  if (e1 >>= 4)
	    {
	      for (j = 0; e1 > 1; j++, e1 >>= 1)
		if (e1 & 1)
		  rv.d *= bigtens[j];
	      /* The last multiplication could overflow. */
	      word0 (rv) -= P * Exp_msk1;
	      rv.d *= bigtens[j];
	      if ((z = word0 (rv) & Exp_mask)
		  > Exp_msk1 * (DBL_MAX_EXP + Bias - P))
		goto ovfl;
	      if (z > Exp_msk1 * (DBL_MAX_EXP + Bias - 1 - P))
		{
		  /* set to largest number */
		  /* (Can't trust DBL_MAX) */
		  word0 (rv) = Big0;
#ifndef _DOUBLE_IS_32BITS
		  word1 (rv) = Big1;
#endif
		}
	      else
		word0 (rv) += P * Exp_msk1;
	    }

	}
    }
  else if (e1 < 0)
    {
      e1 = -e1;
      if ((i = e1 & 15))
	rv.d /= tens[i];
      if (e1 &= ~15)
	{
	  e1 >>= 4;
	  if (e1 >= 1 << n_bigtens)
            goto undfl;
	  for (j = 0; e1 > 1; j++, e1 >>= 1)
	    if (e1 & 1)
	      rv.d *= tinytens[j];
	  /* The last multiplication could underflow. */
	  rv0.d = rv.d;
	  rv.d *= tinytens[j];
	  if (!rv.d)
	    {
	      rv.d = 2. * rv0.d;
	      rv.d *= tinytens[j];
	      if (!rv.d)
		{
		undfl:
		  rv.d = 0.;
		  ptr->_errno = ERANGE;
		  if (bd0)
		    goto retfree;
		  goto ret;
		}
#ifndef _DOUBLE_IS_32BITS
	      word0 (rv) = Tiny0;
	      word1 (rv) = Tiny1;
#else
	      word0 (rv) = Tiny1;
#endif
	      /* The refinement below will clean
	       * this approximation up.
	       */
	    }
	}
    }

  /* Now the hard part -- adjusting rv to the correct value.*/

  /* Put digits into bd: true value = bd * 10^e */

  bd0 = s2b (ptr, s0, nd0, nd, y);

  for (;;)
    {
      bd = Balloc (ptr, bd0->_k);
      Bcopy (bd, bd0);
      bb = d2b (ptr, rv.d, &bbe, &bbbits);	/* rv.d = bb * 2^bbe */
      bs = i2b (ptr, 1);

      if (e >= 0)
	{
	  bb2 = bb5 = 0;
	  bd2 = bd5 = e;
	}
      else
	{
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
      j = 1 + 4 * P - 3 - bbbits + ((bbe + bbbits - 1) & 3);
#else
      j = P + 1 - bbbits;
#endif
#else
      i = bbe + bbbits - 1;	/* logb(rv.d) */
      if (i < Emin)		/* denormal */
	j = bbe + (P - Emin);
      else
	j = P + 1 - bbbits;
#endif
      bb2 += j;
      bd2 += j;
      i = bb2 < bd2 ? bb2 : bd2;
      if (i > bs2)
	i = bs2;
      if (i > 0)
	{
	  bb2 -= i;
	  bd2 -= i;
	  bs2 -= i;
	}
      if (bb5 > 0)
	{
	  bs = pow5mult (ptr, bs, bb5);
	  bb1 = mult (ptr, bs, bb);
	  Bfree (ptr, bb);
	  bb = bb1;
	}
      if (bb2 > 0)
	bb = lshift (ptr, bb, bb2);
      if (bd5 > 0)
	bd = pow5mult (ptr, bd, bd5);
      if (bd2 > 0)
	bd = lshift (ptr, bd, bd2);
      if (bs2 > 0)
	bs = lshift (ptr, bs, bs2);
      delta = diff (ptr, bb, bd);
      dsign = delta->_sign;
      delta->_sign = 0;
      i = cmp (delta, bs);
      if (i < 0)
	{
	  /* Error is less than half an ulp -- check for
	   * special case of mantissa a power of two.
	   */
	  if (dsign || word1 (rv) || word0 (rv) & Bndry_mask)
	    break;
	  delta = lshift (ptr, delta, Log2P);
	  if (cmp (delta, bs) > 0)
	    goto drop_down;
	  break;
	}
      if (i == 0)
	{
	  /* exactly half-way between */
	  if (dsign)
	    {
	      if ((word0 (rv) & Bndry_mask1) == Bndry_mask1
		  && word1 (rv) == 0xffffffff)
		{
		  /*boundary case -- increment exponent*/
		  word0 (rv) = (word0 (rv) & Exp_mask)
		    + Exp_msk1
#ifdef IBM
		    | Exp_msk1 >> 4
#endif
		    ;
#ifndef _DOUBLE_IS_32BITS
		  word1 (rv) = 0;
#endif
		  break;
		}
	    }
	  else if (!(word0 (rv) & Bndry_mask) && !word1 (rv))
	    {
	    drop_down:
	      /* boundary case -- decrement exponent */
#ifdef Sudden_Underflow
	      L = word0 (rv) & Exp_mask;
#ifdef IBM
	      if (L < Exp_msk1)
#else
	      if (L <= Exp_msk1)
#endif
		goto undfl;
	      L -= Exp_msk1;
#else
	      L = (word0 (rv) & Exp_mask) - Exp_msk1;
#endif
	      word0 (rv) = L | Bndry_mask1;
#ifndef _DOUBLE_IS_32BITS
	      word1 (rv) = 0xffffffff;
#endif
#ifdef IBM
	      goto cont;
#else
	      break;
#endif
	    }
#ifndef ROUND_BIASED
	  if (!(word1 (rv) & LSB))
	    break;
#endif
	  if (dsign)
	    rv.d += ulp (rv.d);
#ifndef ROUND_BIASED
	  else
	    {
	      rv.d -= ulp (rv.d);
#ifndef Sudden_Underflow
	      if (!rv.d)
		goto undfl;
#endif
	    }
#endif
	  break;
	}
      if ((aadj = ratio (delta, bs)) <= 2.)
	{
	  if (dsign)
	    aadj = aadj1 = 1.;
	  else if (word1 (rv) || word0 (rv) & Bndry_mask)
	    {
#ifndef Sudden_Underflow
	      if (word1 (rv) == Tiny1 && !word0 (rv))
		goto undfl;
#endif
	      aadj = 1.;
	      aadj1 = -1.;
	    }
	  else
	    {
	      /* special case -- power of FLT_RADIX to be */
	      /* rounded down... */

	      if (aadj < 2. / FLT_RADIX)
		aadj = 1. / FLT_RADIX;
	      else
		aadj *= 0.5;
	      aadj1 = -aadj;
	    }
	}
      else
	{
	  aadj *= 0.5;
	  aadj1 = dsign ? aadj : -aadj;
#ifdef Check_FLT_ROUNDS
	  switch (FLT_ROUNDS)
	    {
	    case 2:		/* towards +infinity */
	      aadj1 -= 0.5;
	      break;
	    case 0:		/* towards 0 */
	    case 3:		/* towards -infinity */
	      aadj1 += 0.5;
	    }
#else
	  if (FLT_ROUNDS == 0)
	    aadj1 += 0.5;
#endif
	}
      y = word0 (rv) & Exp_mask;

      /* Check for overflow */

      if (y == Exp_msk1 * (DBL_MAX_EXP + Bias - 1))
	{
	  rv0.d = rv.d;
	  word0 (rv) -= P * Exp_msk1;
	  adj = aadj1 * ulp (rv.d);
	  rv.d += adj;
	  if ((word0 (rv) & Exp_mask) >=
	      Exp_msk1 * (DBL_MAX_EXP + Bias - P))
	    {
	      if (word0 (rv0) == Big0 && word1 (rv0) == Big1)
		goto ovfl;
#ifdef _DOUBLE_IS_32BITS
	      word0 (rv) = Big1;
#else
	      word0 (rv) = Big0;
	      word1 (rv) = Big1;
#endif
	      goto cont;
	    }
	  else
	    word0 (rv) += P * Exp_msk1;
	}
      else
	{
#ifdef Sudden_Underflow
	  if ((word0 (rv) & Exp_mask) <= P * Exp_msk1)
	    {
	      rv0.d = rv.d;
	      word0 (rv) += P * Exp_msk1;
	      adj = aadj1 * ulp (rv.d);
	      rv.d += adj;
#ifdef IBM
	      if ((word0 (rv) & Exp_mask) < P * Exp_msk1)
#else
	      if ((word0 (rv) & Exp_mask) <= P * Exp_msk1)
#endif
		{
		  if (word0 (rv0) == Tiny0
		      && word1 (rv0) == Tiny1)
		    goto undfl;
		  word0 (rv) = Tiny0;
		  word1 (rv) = Tiny1;
		  goto cont;
		}
	      else
		word0 (rv) -= P * Exp_msk1;
	    }
	  else
	    {
	      adj = aadj1 * ulp (rv.d);
	      rv.d += adj;
	    }
#else
	  /* Compute adj so that the IEEE rounding rules will
	   * correctly round rv.d + adj in some half-way cases.
	   * If rv.d * ulp(rv.d) is denormalized (i.e.,
	   * y <= (P-1)*Exp_msk1), we must adjust aadj to avoid
	   * trouble from bits lost to denormalization;
	   * example: 1.2e-307 .
	   */
	  if (y <= (P - 1) * Exp_msk1 && aadj >= 1.)
	    {
	      aadj1 = (double) (int) (aadj + 0.5);
	      if (!dsign)
		aadj1 = -aadj1;
	    }
	  adj = aadj1 * ulp (rv.d);
	  rv.d += adj;
#endif
	}
      z = word0 (rv) & Exp_mask;
      if (y == z)
	{
	  /* Can we stop now? */
	  L = aadj;
	  aadj -= L;
	  /* The tolerances below are conservative. */
	  if (dsign || word1 (rv) || word0 (rv) & Bndry_mask)
	    {
	      if (aadj < .4999999 || aadj > .5000001)
		break;
	    }
	  else if (aadj < .4999999 / FLT_RADIX)
	    break;
	}
    cont:
      Bfree (ptr, bb);
      Bfree (ptr, bd);
      Bfree (ptr, bs);
      Bfree (ptr, delta);
    }
retfree:
  Bfree (ptr, bb);
  Bfree (ptr, bd);
  Bfree (ptr, bs);
  Bfree (ptr, bd0);
  Bfree (ptr, delta);
ret:
  if (se)
    *se = (char *) s;
  if (digits == 0)
    ptr->_errno = EINVAL;
  return sign ? -rv.d : rv.d;
}

