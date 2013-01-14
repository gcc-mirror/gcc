/* Header used to override things detected by the mk-kinds-h.sh script.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.

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


/* What are the C types corresponding to the real(kind=10) and
   real(kind=16) types? We currently rely on the following assumptions:
     -- if real(kind=10) exists, i.e. if HAVE_GFC_REAL_10 is defined,
        then it is necessarily the "long double" type
     -- if real(kind=16) exists, then:
         * if HAVE_GFC_REAL_10, real(kind=16) is "__float128"
	 * otherwise, real(kind=16) is "long double"
   To allow to change this in the future, we create the
   GFC_REAL_16_IS_FLOAT128 macro that is used throughout libgfortran.  */

#if defined(HAVE_GFC_REAL_16)
# if defined(HAVE_GFC_REAL_10)
#  define GFC_REAL_16_IS_FLOAT128
#  if !defined(HAVE_FLOAT128)
#   error "Where has __float128 gone?"
#  endif
# else
#  define GFC_REAL_16_IS_LONG_DOUBLE
# endif
#endif

