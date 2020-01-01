/* Implementation of the ERFC_SCALED intrinsic, to be included by erfc_scaled.c
   Copyright (C) 2008-2020 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR a PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This implementation of ERFC_SCALED is based on the netlib algorithm
   available at http://www.netlib.org/specfun/erf  */

#define TYPE KIND_SUFFIX(GFC_REAL_,KIND)
#define CONCAT(x,y) x ## y
#define KIND_SUFFIX(x,y) CONCAT(x,y)

#if (KIND == 4)

# define EXP(x) expf(x)
# define TRUNC(x) truncf(x)

#elif (KIND == 8)

# define EXP(x) exp(x)
# define TRUNC(x) trunc(x)

#elif (KIND == 10)

# ifdef HAVE_EXPL
#  define EXP(x) expl(x)
# endif
# ifdef HAVE_TRUNCL
#  define TRUNC(x) truncl(x)
# endif

#else

# error "What exactly is it that you want me to do?"

#endif

#if defined(EXP) && defined(TRUNC)

extern TYPE KIND_SUFFIX(erfc_scaled_r,KIND) (TYPE);
export_proto(KIND_SUFFIX(erfc_scaled_r,KIND));

TYPE
KIND_SUFFIX(erfc_scaled_r,KIND) (TYPE x)
{
  /* The main computation evaluates near-minimax approximations
     from "Rational Chebyshev approximations for the error function"
     by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
     transportable program uses rational functions that theoretically
     approximate  erf(x)  and  erfc(x)  to at least 18 significant
     decimal digits.  The accuracy achieved depends on the arithmetic
     system, the compiler, the intrinsic functions, and proper
     selection of the machine-dependent constants.  */

  int i;
  TYPE del, res, xden, xnum, y, ysq;

#if (KIND == 4)
  static TYPE xneg = -9.382, xsmall = 5.96e-8,
	      xbig = 9.194, xhuge = 2.90e+3, xmax = 4.79e+37;
#else
  static TYPE xneg = -26.628, xsmall = 1.11e-16,
	      xbig = 26.543, xhuge = 6.71e+7, xmax = 2.53e+307;
#endif

#define SQRPI ((TYPE) 0.56418958354775628695L)
#define THRESH ((TYPE) 0.46875L)

  static TYPE a[5] = { 3.16112374387056560l, 113.864154151050156l,
    377.485237685302021l, 3209.37758913846947l, 0.185777706184603153l };

  static TYPE b[4] = { 23.6012909523441209l, 244.024637934444173l,
    1282.61652607737228l, 2844.23683343917062l };

  static TYPE c[9] = { 0.564188496988670089l, 8.88314979438837594l,
    66.1191906371416295l, 298.635138197400131l, 881.952221241769090l,
    1712.04761263407058l, 2051.07837782607147l, 1230.33935479799725l,
    2.15311535474403846e-8l };

  static TYPE d[8] = { 15.7449261107098347l, 117.693950891312499l,
    537.181101862009858l, 1621.38957456669019l, 3290.79923573345963l,
    4362.61909014324716l, 3439.36767414372164l, 1230.33935480374942l };

  static TYPE p[6] = { 0.305326634961232344l, 0.360344899949804439l,
    0.125781726111229246l, 0.0160837851487422766l,
    0.000658749161529837803l, 0.0163153871373020978l };

  static TYPE q[5] = { 2.56852019228982242l, 1.87295284992346047l,
    0.527905102951428412l, 0.0605183413124413191l,
    0.00233520497626869185l };

  y = (x > 0 ? x : -x);
  if (y <= THRESH)
    {
      ysq = 0;
      if (y > xsmall)
	ysq = y * y;
      xnum = a[4]*ysq;
      xden = ysq;
      for (i = 0; i <= 2; i++)
	{
          xnum = (xnum + a[i]) * ysq;
          xden = (xden + b[i]) * ysq;
	}
      res = x * (xnum + a[3]) / (xden + b[3]);
      res = 1 - res;
      res = EXP(ysq) * res;
      return res;
    }
  else if (y <= 4)
    {
      xnum = c[8]*y;
      xden = y;
      for (i = 0; i <= 6; i++)
	{
	  xnum = (xnum + c[i]) * y;
	  xden = (xden + d[i]) * y;
	}
      res = (xnum + c[7]) / (xden + d[7]);
    }
  else
    {
      res = 0;
      if (y >= xbig)
	{
          if (y >= xmax)
	    goto finish;
          if (y >= xhuge)
	    {
	      res = SQRPI / y;
	      goto finish;
	    }
	}
      ysq = ((TYPE) 1) / (y * y);
      xnum = p[5]*ysq;
      xden = ysq;
      for (i = 0; i <= 3; i++)
	{
          xnum = (xnum + p[i]) * ysq;
          xden = (xden + q[i]) * ysq;
	}
      res = ysq *(xnum + p[4]) / (xden + q[4]);
      res = (SQRPI -  res) / y;
    }

finish:
  if (x < 0)
    {
      if (x < xneg)
	res = __builtin_inf ();
      else
	{
	  ysq = TRUNC (x*((TYPE) 16))/((TYPE) 16);
	  del = (x-ysq)*(x+ysq);
	  y = EXP(ysq*ysq) * EXP(del);
	  res = (y+y) - res;
	}
    }
  return res;
}

#endif

#undef EXP
#undef TRUNC

#undef CONCAT
#undef TYPE
#undef KIND_SUFFIX
