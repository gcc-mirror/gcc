/* Implementation of the ERFC_SCALED intrinsic.
   Copyright (C) 2008-2025 Free Software Foundation, Inc.

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

#include "libgfortran.h"

/* This implementation of ERFC_SCALED is based on the netlib algorithm
   available at http://www.netlib.org/specfun/erf  */

#ifdef HAVE_GFC_REAL_4
#undef KIND
#define KIND 4
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_8
#undef KIND
#define KIND 8
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_10
#undef KIND
#define KIND 10
#include "erfc_scaled_inc.c"
#endif

#ifdef HAVE_GFC_REAL_16

/* For quadruple-precision, netlib's implementation is
   not accurate enough.  We provide another one.  */

#ifdef GFC_REAL_16_IS_FLOAT128

# ifdef GFC_REAL_16_USE_IEC_60559
#  define _THRESH -106.566990228185312813205074546585730F128
#  define _M_2_SQRTPI M_2_SQRTPIf128
#  define _INF __builtin_inff128()
#  define _ERFC(x) erfcf128(x)
#  define _EXP(x) expf128(x)
# else
#  define _THRESH -106.566990228185312813205074546585730Q
#  define _M_2_SQRTPI M_2_SQRTPIq
#  define _INF __builtin_infq()
#  define _ERFC(x) erfcq(x)
#  define _EXP(x) expq(x)
# endif

#else

# define _THRESH -106.566990228185312813205074546585730L
# ifndef M_2_SQRTPIl
#  define M_2_SQRTPIl 1.128379167095512573896158903121545172L
# endif
# define _M_2_SQRTPI M_2_SQRTPIl
# define _INF __builtin_infl()
# ifdef HAVE_ERFCL
#  define _ERFC(x) erfcl(x)
# endif
# ifdef HAVE_EXPL
#  define _EXP(x) expl(x)
# endif

#endif

#define ERFC_SCALED(k) \
GFC_REAL_ ## k								    \
erfc_scaled_r ## k (GFC_REAL_ ## k x)					    \
{									    \
  if (x < _THRESH)							    \
    {									    \
      return _INF;							    \
    }									    \
  if (x < 12)								    \
    {									    \
      /* Compute directly as ERFC_SCALED(x) = ERFC(x) * EXP(X**2).	    \
	 This is not perfect, but much better than netlib.  */		    \
      return _ERFC(x) * _EXP(x * x);					    \
    }									    \
  else									    \
    {									    \
      /* Calculate ERFC_SCALED(x) using a power series in 1/x:		    \
	 ERFC_SCALED(x) = 1 / (x * sqrt(pi))				    \
			 * (1 + Sum_n (-1)**n * (1 * 3 * 5 * ... * (2n-1))  \
					      / (2 * x**2)**n)		    \
       */								    \
      GFC_REAL_ ## k sum = 0, oldsum;					    \
      GFC_REAL_ ## k inv2x2 = 1 / (2 * x * x);				    \
      GFC_REAL_ ## k fac = 1;						    \
      int n = 1;							    \
									    \
      while (n < 200)							    \
	{								    \
	  fac *= - (2*n - 1) * inv2x2;					    \
	  oldsum = sum;							    \
	  sum += fac;							    \
									    \
	  if (sum == oldsum)						    \
	    break;							    \
									    \
	  n++;								    \
	}								    \
									    \
      return (1 + sum) / x * (_M_2_SQRTPI / 2);				    \
    }									    \
}

#if defined(_ERFC) && defined(_EXP)

extern GFC_REAL_16 erfc_scaled_r16 (GFC_REAL_16);
export_proto(erfc_scaled_r16);

ERFC_SCALED(16)

#endif

#undef _THRESH
#undef _M_2_SQRTPI
#undef _INF
#undef _ERFC
#undef _EXP

#endif

#ifdef HAVE_GFC_REAL_17

/* For quadruple-precision, netlib's implementation is
   not accurate enough.  We provide another one.  */

# define _THRESH GFC_REAL_17_LITERAL(-106.566990228185312813205074546585730)
# define _M_2_SQRTPI GFC_REAL_17_LITERAL(M_2_SQRTPI)
# define _INF __builtin_inff128()
# ifdef POWER_IEEE128
#  define _ERFC(x) __erfcieee128(x)
#  define _EXP(x) __expieee128(x)
# elif defined(GFC_REAL_17_USE_IEC_60559)
#  define _ERFC(x) erfcf128(x)
#  define _EXP(x) expf128(x)
# else
#  define _ERFC(x) erfcq(x)
#  define _EXP(x) expq(x)
# endif

extern GFC_REAL_17 erfc_scaled_r17 (GFC_REAL_17);
export_proto(erfc_scaled_r17);

ERFC_SCALED(17)

#undef _THRESH
#undef _M_2_SQRTPI
#undef _INF
#undef _ERFC
#undef _EXP
#undef ERFC_SCALED

#endif
