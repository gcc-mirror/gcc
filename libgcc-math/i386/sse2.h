/* This re-writes the math routines in flt-32 and dbl-64 to have their
   exported symbols prefix with __libm_sse2 for an SSE2 ABI implementation.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "libc-symbols.h"

#undef weak_alias
#undef strong_alias
#undef hidden_def
#define weak_alias(a,b)
#define strong_alias(a,b)
#define hidden_def(a)

/* s_atanf.c */
#define __atanf __libm_sse2_atanf

/* e_asinf.c */
#define __ieee754_acosf __libm_sse2_acosf
#define __ieee754_asinf __libm_sse2_asinf

/* e_atan2f.c */
#define __ieee754_atan2f __libm_sse2_atan2f
#define __ieee754_expf __libm_sse2_expf

/* e_log10f.c */
#define __ieee754_log10f __libm_sse2_log10f

/* e_logf.c */
#define __ieee754_logf __libm_sse2_logf

/* e_powf.c */
#define __ieee754_powf __libm_sse2_powf

/* s_sinf.c */
#define __sinf __libm_sse2_sinf
#define __cosf __libm_sse2_cosf

/* s_tanf.c */
#define __tanf __libm_sse2_tanf

/* s_atan.c */
#define atan __libm_sse2_atan

/* e_asin.c */
#define __ieee754_acos __libm_sse2_acos
#define __ieee754_asin __libm_sse2_asin

/* e_atan2.c */
#define __ieee754_atan2 __libm_sse2_atan2

/* e_exp.c */
#define __ieee754_exp __libm_sse2_exp

/* e_log10.c */
#define __ieee754_log10 __libm_sse2_log10

/* e_log.c */
#define __ieee754_log __libm_sse2_log

/* e_pow.c */
#define __ieee754_pow __libm_sse2_pow

/* s_sin.c */
#define __cos __libm_sse2_cos
#define __sin __libm_sse2_sin

/* s_tan.c */
#define tan __libm_sse2_tan
