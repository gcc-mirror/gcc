/* Implementation of the degree trignometric functions COSD, SIND, TAND.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargl@gcc.gnu.org>
   and Fritz Reese <foreese@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

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


/*

This file is included from both the FE and the runtime library code.
Operations are generalized using GMP/MPFR functions. When included from
libgfortran, these should be overridden using macros which will use native
operations conforming to the same API. From the FE, the GMP/MPFR functions can
be used as-is.

The following macros are used and must be defined, unless listed as [optional]:

FTYPE
    Type name for the real-valued parameter.
    Variables of this type are constructed/destroyed using mpfr_init()
    and mpfr_clear.

RETTYPE
    Return type of the functions.

RETURN(x)
    Insert code to return a value.
    The parameter x is the result variable, which was also the input parameter.

ITYPE
    Type name for integer types.

SIND, COSD, TRIGD
    Names for the degree-valued trig functions defined by this module.

ENABLE_SIND, ENABLE_COSD, ENABLE_TAND
    Whether the degree-valued trig functions can be enabled.

ERROR_RETURN(f, k, x)
    If ENABLE_<xxx>D is not defined, this is substituted to assert an
    error condition for function f, kind k, and parameter x.
    The function argument is one of {sind, cosd, tand}.

ISFINITE(x)
    Whether x is a regular number or zero (not inf or NaN).

D2R(x)
    Convert x from radians to degrees.

SET_COSD30(x)
    Set x to COSD(30), or equivalently, SIND(60).

TINY_LITERAL [optional]
    Value subtracted from 1 to cause raise INEXACT for COSD(x) for x << 1.
    If not set, COSD(x) for x <= COSD_SMALL_LITERAL simply returns 1.

COSD_SMALL_LITERAL [optional]
    Value such that x <= COSD_SMALL_LITERAL implies COSD(x) = 1 to within the
    precision of FTYPE. If not set, this condition is not checked.

SIND_SMALL_LITERAL [optional]
    Value such that x <= SIND_SMALL_LITERAL implies SIND(x) = D2R(x) to within
    the precision of FTYPE. If not set, this condition is not checked.

*/


#ifdef SIND
/* Compute sind(x) = sin(x * pi / 180). */

RETTYPE
SIND (FTYPE x)
{
#ifdef ENABLE_SIND
  if (ISFINITE (x))
    {
      FTYPE s, one;

      /* sin(-x) = - sin(x).  */
      mpfr_init (s);
      mpfr_init_set_ui (one, 1, GFC_RND_MODE);
      mpfr_copysign (s, one, x, GFC_RND_MODE);
      mpfr_clear (one);

#ifdef SIND_SMALL_LITERAL
      /* sin(x) = x as x -> 0; but only for some precisions. */
      FTYPE ax;
      mpfr_init (ax);
      mpfr_abs (ax, x, GFC_RND_MODE);
      if (mpfr_cmp_ld (ax, SIND_SMALL_LITERAL) < 0)
	{
	  D2R (x);
	  mpfr_clear (ax);
	  return x;
	}

      mpfr_swap (x, ax);
      mpfr_clear (ax);

#else
      mpfr_abs (x, x, GFC_RND_MODE);
#endif /* SIND_SMALL_LITERAL */

      /* Reduce angle to x in [0,360].  */
      FTYPE period;
      mpfr_init_set_ui (period, 360, GFC_RND_MODE);
      mpfr_fmod (x, x, period, GFC_RND_MODE);
      mpfr_clear (period);

      /* Special cases with exact results.  */
      ITYPE n;
      mpz_init (n);
      if (mpfr_get_z (n, x, GFC_RND_MODE) == 0 && mpz_divisible_ui_p (n, 30))
	{
	  /* Flip sign for odd n*pi (x is % 360 so this is only for 180).
	     This respects sgn(sin(x)) = sgn(d/dx sin(x)) = sgn(cos(x)). */
	  if (mpz_divisible_ui_p (n, 180))
	    {
	      mpfr_set_ui (x, 0, GFC_RND_MODE);
	      if (mpz_cmp_ui (n, 180) == 0)
		mpfr_neg (s, s, GFC_RND_MODE);
	    }
	  else if (mpz_divisible_ui_p (n, 90))
	    mpfr_set_si (x, (mpz_cmp_ui (n, 90) == 0 ? 1 : -1), GFC_RND_MODE);
	  else if (mpz_divisible_ui_p (n, 60))
	    {
	      SET_COSD30 (x);
	      if (mpz_cmp_ui (n, 180) >= 0)
		mpfr_neg (x, x, GFC_RND_MODE);
	    }
	  else
	    mpfr_set_ld (x, (mpz_cmp_ui (n, 180) < 0 ? 0.5L : -0.5L),
			 GFC_RND_MODE);
	}

      /* Fold [0,360] into the range [0,45], and compute either SIN() or
         COS() depending on symmetry of shifting into the [0,45] range.  */
      else
	{
	  bool fold_cos = false;
	  if (mpfr_cmp_ui (x, 180) <= 0)
	    {
	      if (mpfr_cmp_ui (x, 90) <= 0)
		{
		  if (mpfr_cmp_ui (x, 45) > 0)
		    {
		      /* x = COS(D2R(90 - x)) */
		      mpfr_ui_sub (x, 90, x, GFC_RND_MODE);
		      fold_cos = true;
		    }
		}
	      else
		{
		  if (mpfr_cmp_ui (x, 135) <= 0)
		    {
		      mpfr_sub_ui (x, x, 90, GFC_RND_MODE);
		      fold_cos = true;
		    }
		  else
		    mpfr_ui_sub (x, 180, x, GFC_RND_MODE);
		}
	    }

	  else if (mpfr_cmp_ui (x, 270) <= 0)
	    {
	      if (mpfr_cmp_ui (x, 225) <= 0)
		mpfr_sub_ui (x, x, 180, GFC_RND_MODE);
	      else
		{
		  mpfr_ui_sub (x, 270, x, GFC_RND_MODE);
		  fold_cos = true;
		}
	      mpfr_neg (s, s, GFC_RND_MODE);
	    }

	  else
	    {
	      if (mpfr_cmp_ui (x, 315) <= 0)
		{
		  mpfr_sub_ui (x, x, 270, GFC_RND_MODE);
		  fold_cos = true;
		}
	      else
		mpfr_ui_sub (x, 360, x, GFC_RND_MODE);
	      mpfr_neg (s, s, GFC_RND_MODE);
	    }

	  D2R (x);

	  if (fold_cos)
	    mpfr_cos (x, x, GFC_RND_MODE);
	  else
	    mpfr_sin (x, x, GFC_RND_MODE);
	}

      mpfr_mul (x, x, s, GFC_RND_MODE);
      mpz_clear (n);
      mpfr_clear (s);
    }

  /* Return NaN for +-Inf and NaN and raise exception.  */
  else
    mpfr_sub (x, x, x, GFC_RND_MODE);

  RETURN (x);

#else
  ERROR_RETURN(sind, KIND, x);
#endif // ENABLE_SIND
}
#endif // SIND


#ifdef COSD
/* Compute cosd(x) = cos(x * pi / 180).  */

RETTYPE
COSD (FTYPE x)
{
#ifdef ENABLE_COSD
#if defined(TINY_LITERAL) && defined(COSD_SMALL_LITERAL)
  static const volatile FTYPE tiny = TINY_LITERAL;
#endif

  if (ISFINITE (x))
    {
#ifdef COSD_SMALL_LITERAL
      FTYPE ax;
      mpfr_init (ax);

      mpfr_abs (ax, x, GFC_RND_MODE);
      /* No spurious underflows!.  In radians, cos(x) = 1-x*x/2 as x -> 0.  */
      if (mpfr_cmp_ld (ax, COSD_SMALL_LITERAL) <= 0)
	{
	  mpfr_set_ui (x, 1, GFC_RND_MODE);
#ifdef TINY_LITERAL
	  /* Cause INEXACT.  */
	  if (!mpfr_zero_p (ax))
	    mpfr_sub_d (x, x, tiny, GFC_RND_MODE);
#endif

	  mpfr_clear (ax);
	  return x;
	}

      mpfr_swap (x, ax);
      mpfr_clear (ax);
#else
      mpfr_abs (x, x, GFC_RND_MODE);
#endif /* COSD_SMALL_LITERAL */

      /* Reduce angle to ax in [0,360].  */
      FTYPE period;
      mpfr_init_set_ui (period, 360, GFC_RND_MODE);
      mpfr_fmod (x, x, period, GFC_RND_MODE);
      mpfr_clear (period);

      /* Special cases with exact results.
         Return negative zero for cosd(270) for consistency with libm cos().  */
      ITYPE n;
      mpz_init (n);
      if (mpfr_get_z (n, x, GFC_RND_MODE) == 0 && mpz_divisible_ui_p (n, 30))
	{
	  if (mpz_divisible_ui_p (n, 180))
	    mpfr_set_si (x, (mpz_cmp_ui (n, 180) == 0 ? -1 : 1),
			 GFC_RND_MODE);
	  else if (mpz_divisible_ui_p (n, 90))
	    mpfr_set_zero (x, 0);
	  else if (mpz_divisible_ui_p (n, 60))
	    {
	      mpfr_set_ld (x, 0.5, GFC_RND_MODE);
	      if (mpz_cmp_ui (n, 60) != 0 && mpz_cmp_ui (n, 300) != 0)
		mpfr_neg (x, x, GFC_RND_MODE);
	    }
	  else
	    {
	      SET_COSD30 (x);
	      if (mpz_cmp_ui (n, 30) != 0 && mpz_cmp_ui (n, 330) != 0)
		mpfr_neg (x, x, GFC_RND_MODE);
	    }
	}

      /* Fold [0,360] into the range [0,45], and compute either SIN() or
         COS() depending on symmetry of shifting into the [0,45] range.  */
      else
	{
	  bool neg = false;
	  bool fold_sin = false;
	  if (mpfr_cmp_ui (x, 180) <= 0)
	    {
	      if (mpfr_cmp_ui (x, 90) <= 0)
		{
		  if (mpfr_cmp_ui (x, 45) > 0)
		    {
		      mpfr_ui_sub (x, 90, x, GFC_RND_MODE);
		      fold_sin = true;
		    }
		}
	      else
		{
		  if (mpfr_cmp_ui (x, 135) <= 0)
		    {
		      mpfr_sub_ui (x, x, 90, GFC_RND_MODE);
		      fold_sin = true;
		    }
		  else
		    mpfr_ui_sub (x, 180, x, GFC_RND_MODE);
		  neg = true;
		}
	    }

	  else if (mpfr_cmp_ui (x, 270) <= 0)
	    {
	      if (mpfr_cmp_ui (x, 225) <= 0)
		mpfr_sub_ui (x, x, 180, GFC_RND_MODE);
	      else
		{
		  mpfr_ui_sub (x, 270, x, GFC_RND_MODE);
		  fold_sin = true;
		}
	      neg = true;
	    }

	  else
	    {
	      if (mpfr_cmp_ui (x, 315) <= 0)
		{
		  mpfr_sub_ui (x, x, 270, GFC_RND_MODE);
		  fold_sin = true;
		}
	      else
		mpfr_ui_sub (x, 360, x, GFC_RND_MODE);
	    }

	  D2R (x);

	  if (fold_sin)
	    mpfr_sin (x, x, GFC_RND_MODE);
	  else
	    mpfr_cos (x, x, GFC_RND_MODE);

	  if (neg)
	    mpfr_neg (x, x, GFC_RND_MODE);
	}

      mpz_clear (n);
    }

  /* Return NaN for +-Inf and NaN and raise exception.  */
  else
    mpfr_sub (x, x, x, GFC_RND_MODE);

  RETURN (x);

#else
  ERROR_RETURN(cosd, KIND, x);
#endif // ENABLE_COSD
}
#endif // COSD


#ifdef TAND
/* Compute tand(x) = tan(x * pi / 180).  */

RETTYPE
TAND (FTYPE x)
{
#ifdef ENABLE_TAND
  if (ISFINITE (x))
    {
      FTYPE s, one;

      /* tan(-x) = - tan(x).  */
      mpfr_init (s);
      mpfr_init_set_ui (one, 1, GFC_RND_MODE);
      mpfr_copysign (s, one, x, GFC_RND_MODE);
      mpfr_clear (one);

#ifdef SIND_SMALL_LITERAL
      /* tan(x) = x as x -> 0; but only for some precisions. */
      FTYPE ax;
      mpfr_init (ax);
      mpfr_abs (ax, x, GFC_RND_MODE);
      if (mpfr_cmp_ld (ax, SIND_SMALL_LITERAL) < 0)
	{
	  D2R (x);
	  mpfr_clear (ax);
	  return x;
	}

      mpfr_swap (x, ax);
      mpfr_clear (ax);

#else
      mpfr_abs (x, x, GFC_RND_MODE);
#endif /* SIND_SMALL_LITERAL */

      /* Reduce angle to x in [0,360].  */
      FTYPE period;
      mpfr_init_set_ui (period, 360, GFC_RND_MODE);
      mpfr_fmod (x, x, period, GFC_RND_MODE);
      mpfr_clear (period);

      /* Special cases with exact results. */
      ITYPE n;
      mpz_init (n);
      if (mpfr_get_z (n, x, GFC_RND_MODE) == 0 && mpz_divisible_ui_p (n, 45))
	{
	  if (mpz_divisible_ui_p (n, 180))
	    mpfr_set_zero (x, 0);

	  /* Though mathematically NaN is more appropriate for tan(n*90),
	     returning +/-Inf offers the advantage that 1/tan(n*90) returns 0,
	     which is mathematically sound. In fact we rely on this behavior
	     to implement COTAND(x) = 1 / TAND(x).
	   */
	  else if (mpz_divisible_ui_p (n, 90))
	    mpfr_set_inf (x, mpz_cmp_ui (n, 90) == 0 ? 0 : 1);

	  else
	    {
	      mpfr_set_ui (x, 1, GFC_RND_MODE);
	      if (mpz_cmp_ui (n, 45) != 0 && mpz_cmp_ui (n, 225) != 0)
		mpfr_neg (x, x, GFC_RND_MODE);
	    }
	}

      else
	{
	  /* Fold [0,360] into the range [0,90], and compute TAN().  */
	  if (mpfr_cmp_ui (x, 180) <= 0)
	    {
	      if (mpfr_cmp_ui (x, 90) > 0)
		{
		  mpfr_ui_sub (x, 180, x, GFC_RND_MODE);
		  mpfr_neg (s, s, GFC_RND_MODE);
		}
	    }
	  else
	    {
	      if (mpfr_cmp_ui (x, 270) <= 0)
		{
		  mpfr_sub_ui (x, x, 180, GFC_RND_MODE);
		}
	      else
		{
		  mpfr_ui_sub (x, 360, x, GFC_RND_MODE);
		  mpfr_neg (s, s, GFC_RND_MODE);
		}
	    }

	  D2R (x);
	  mpfr_tan (x, x, GFC_RND_MODE);
	}

      mpfr_mul (x, x, s, GFC_RND_MODE);
      mpz_clear (n);
      mpfr_clear (s);
    }

  /* Return NaN for +-Inf and NaN and raise exception.  */
  else
    mpfr_sub (x, x, x, GFC_RND_MODE);

  RETURN (x);

#else
  ERROR_RETURN(tand, KIND, x);
#endif // ENABLE_TAND
}
#endif // TAND

/* vim: set ft=c: */
