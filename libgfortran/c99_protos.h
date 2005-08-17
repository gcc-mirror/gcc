/* Declarations of various C99 functions 
   Copyright (C) 2004 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfortran; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifndef C99_PROTOS_H
#define C99_PROTOS_H

#ifndef HAVE_ACOSF
extern float acosf(float);
#endif

#ifndef HAVE_ACOSHF
extern float acoshf(float);
#endif

#ifndef HAVE_ASINF
extern float asinf(float);
#endif

#ifndef HAVE_ASINHF
extern float asinhf(float);
#endif

#ifndef HAVE_ATAN2F
extern float atan2f(float, float);
#endif

#ifndef HAVE_ATANF
extern float atanf(float);
#endif

#ifndef HAVE_ATANHF
extern float atanhf(float);
#endif

#ifndef HAVE_CEILF
extern float ceilf(float);
#endif

#ifndef HAVE_COPYSIGNF
extern float copysignf(float, float);
#endif

#ifndef HAVE_COSF
extern float cosf(float);
#endif

#ifndef HAVE_COSHF
extern float coshf(float);
#endif

#ifndef HAVE_EXPF
extern float expf(float);
#endif

#ifndef HAVE_FABSF
extern float fabsf(float);
#endif

#ifndef HAVE_FLOORF
extern float floorf(float);
#endif

#ifndef HAVE_FREXPF
extern float frexpf(float, int *);
#endif

#ifndef HAVE_HYPOTF
extern float hypotf(float, float);
#endif

#ifndef HAVE_LOGF
extern float logf(float);
#endif

#ifndef HAVE_LOG10F
extern float log10f(float);
#endif

#ifndef HAVE_SCALBN
extern double scalbn(double, int);
#endif

#ifndef HAVE_SCALBNF
extern float scalbnf(float, int);
#endif

#ifndef HAVE_SINF
extern float sinf(float);
#endif

#ifndef HAVE_SINHF
extern float sinhf(float);
#endif

#ifndef HAVE_SQRTF
extern float sqrtf(float);
#endif

#ifndef HAVE_TANF
extern float tanf(float);
#endif

#ifndef HAVE_TANHF
extern float tanhf(float);
#endif

#ifndef HAVE_TRUNC
extern double trunc(double x);
#endif

#ifndef HAVE_TRUNCF
extern float truncf(float x);
#endif

#ifndef HAVE_NEXTAFTERF
extern float nextafterf(float, float);
#endif

#ifndef HAVE_POWF
extern float powf(float, float);
#endif

#ifndef HAVE_ROUND
extern double round(double);
#endif

#ifndef HAVE_ROUNDF
extern float roundf(float);
#endif

#ifndef HAVE_LOG10L
extern long double log10l(long double);
#endif

#endif  /* C99_PROTOS_H  */

