/* Copyright The GNU Toolchain Authors. */

/* This file is part of the GNU COBOL runtime library (libgcobol).

libgcobol is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

libgcobol is distributed in the hope that it will be useful,
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

/* You must include "config.h" before this file.  */

#if __LDBL_MANT_DIG__ == 113 && __LDBL_MIN_EXP__ == -16381
// Use long double, l suffix on calls, l or L suffix in literals
# define GCOB_FP128 long double
# define GCOB_FP128_LITERAL(lit) (lit ## l)
# define FP128_FUNC(funcname) funcname ## l
# define FP128_FMT "L"
# define strtofp128(nptr, endptr) strtold(nptr, endptr)
# define strfromfp128(str, n, format, fp) snprintf(str, n, format, fp)
#elif __FLT128_MANT_DIG__ == 113 && __FLT128_MIN_EXP__ == -16381 \
     && defined(USE_IEC_60559)
// Use _Float128, f128 suffix on calls, f128 or F128 suffix on literals
# define GCOB_FP128 _Float128
# define GCOB_FP128_LITERAL(lit) (lit ## f128)
# define FP128_FUNC(funcname) funcname ## f128
# define FP128_FMT ""
# define strtofp128(nptr, endptr) strtof128(nptr, endptr)
# define strfromfp128(str, n, format, fp) strfromf128(str, n, format, fp)
#elif __FLT128_MANT_DIG__ == 113 && __FLT128_MIN_EXP__ == -16381
// Use __float128, q suffix on calls, q or Q suffix on literals
# define GCOB_FP128 __float128
# define GCOB_FP128_LITERAL(lit) (lit ## q)
# define FP128_FUNC(funcname) funcname ## q
# define FP128_FMT "Q"
# define strtofp128(nptr, endptr) strtoflt128(nptr, endptr)
# define strfromfp128(str, n, format, fp) quadmath_snprintf(str, n, format, fp)
#else
# error "libgcobol requires 128b floating point"
#endif

#if USE_QUADMATH
/* We will assume that unless we found the 128 to/from string and some
   representative trig functions, we need libquadmath to support those.  */
# include "quadmath.h"
#endif
