/* s_ceill.c -- long double version of s_ceil.c.
 * Conversion to IEEE quad long double by Jakub Jelinek, jj@ultra.linux.cz.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

/*
 * ceill(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *	Bit twiddling.
 */

long double
ceill (long double x)
{
  int64_t i0, i1, j0;
  uint64_t i, j;

  GET_LDOUBLE_WORDS64 (i0, i1, x);
  j0 = ((i0 >> 48) & 0x7fff) - 0x3fff;
  if (j0 < 48)
    {
      if (j0 < 0)
	{
	  /* return 0*sign(x) if |x|<1 */
	  if (i0 < 0)
	    {
	      i0 = 0x8000000000000000ULL;
	      i1 = 0;
	    }
	  else if ((i0|i1) != 0)
	    {
	      i0 = 0x3fff000000000000ULL;
	      i1 = 0;
	    }
	}
      else
	{
	  i = (0x0000ffffffffffffULL) >> j0;
	  if (((i0 & i) | i1) == 0)
	    /* x is integral */
	    return x;
	  if (i0 > 0)
	    i0 += (0x0001000000000000LL) >> j0;
	  i0 &= (~i);
	  i1 = 0;
	}
    }
  else if (j0 > 111)
    {
      if (j0 == 0x4000)
	/* inf or NaN */
	return x+x;
      else
	/* x is integral */
	return x;
    }
  else
    {
      i = -1ULL >> (j0 - 48);
      if ((i1 & i) == 0)
	/* x is integral */
	return x;
      if (i0 > 0)
	{
	  if (j0 == 48)
	    i0 += 1;
	  else
	    {
	      j = i1 + (1LL << (112 - j0));
	      if ((int64_t) j < i1)
		/* got a carry */
		i0 += 1;
	      i1 = j;
	    }
	}
      i1 &= (~i);
    }
  SET_LDOUBLE_WORDS64 (x, i0, i1);
  return x;
}
