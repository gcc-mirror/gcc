/* Header used to override things detected by the mk-kinds-h.sh script.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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


/* Ensure that TFmode is available under.  */

#if defined(GFC_REAL_16_IS_FLOAT128) && !defined(HAVE_FLOAT128)
# error "Where has _Float128 gone?"
#endif

/* Keep these conditions on one line so grep can filter it out.  */
#if defined(__powerpc64__)  && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__  && __SIZEOF_LONG_DOUBLE__ == 16
typedef _Float128 GFC_REAL_17;
typedef _Complex _Float128 GFC_COMPLEX_17;
#define HAVE_GFC_REAL_17
#define HAVE_GFC_COMPLEX_17
#define GFC_REAL_17_IS_FLOAT128
#ifdef USE_IEC_60559
#define GFC_REAL_17_USE_IEC_60559
#define GFC_REAL_17_HUGE 1.18973149535723176508575932662800702e4932f128
#define GFC_REAL_17_LITERAL_SUFFIX f128
#define GFC_REAL_17_LITERAL(X) (X ## f128)
#else
#define GFC_REAL_17_HUGE 1.18973149535723176508575932662800702e4932q
#define GFC_REAL_17_LITERAL_SUFFIX q
#define GFC_REAL_17_LITERAL(X) (X ## q)
#endif
#define GFC_REAL_17_DIGITS 113
#define GFC_REAL_17_RADIX 2
#endif
