`/* Implementation of the MATMUL intrinsic
   Copyright (C) 2002-2019 Free Software Foundation, Inc.
   Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>.

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
#include <string.h>
#include <assert.h>'

include(iparm.m4)dnl

/* These are the specific versions of matmul with -mprefer-avx128.  */

`#if defined (HAVE_'rtype_name`)

/* Prototype for the BLAS ?gemm subroutine, a pointer to which can be
   passed to us by the front-end, in which case we call it for large
   matrices.  */

typedef void (*blas_call)(const char *, const char *, const int *, const int *,
                          const int *, const 'rtype_name` *, const 'rtype_name` *,
                          const int *, const 'rtype_name` *, const int *,
                          const 'rtype_name` *, 'rtype_name` *, const int *,
                          int, int);

#if defined(HAVE_AVX) && defined(HAVE_FMA3) && defined(HAVE_AVX128)
'define(`matmul_name',`matmul_'rtype_code`_avx128_fma3')dnl
`void
'matmul_name` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx,fma")));
internal_proto('matmul_name`);
'include(matmul_internal.m4)dnl
`#endif

#if defined(HAVE_AVX) && defined(HAVE_FMA4) && defined(HAVE_AVX128)
'define(`matmul_name',`matmul_'rtype_code`_avx128_fma4')dnl
`void
'matmul_name` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx,fma4")));
internal_proto('matmul_name`);
'include(matmul_internal.m4)dnl
`#endif

#endif
'
