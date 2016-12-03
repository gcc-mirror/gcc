`/* Implementation of the MATMUL intrinsic
   Copyright (C) 2002-2016 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
#include <stdlib.h>
#include <string.h>
#include <assert.h>'

include(iparm.m4)dnl

`#if defined (HAVE_'rtype_name`)

/* Prototype for the BLAS ?gemm subroutine, a pointer to which can be
   passed to us by the front-end, in which case we call it for large
   matrices.  */

typedef void (*blas_call)(const char *, const char *, const int *, const int *,
                          const int *, const 'rtype_name` *, const 'rtype_name` *,
                          const int *, const 'rtype_name` *, const int *,
                          const 'rtype_name` *, 'rtype_name` *, const int *,
                          int, int);

/* The order of loops is different in the case of plain matrix
   multiplication C=MATMUL(A,B), and in the frequent special case where
   the argument A is the temporary result of a TRANSPOSE intrinsic:
   C=MATMUL(TRANSPOSE(A),B).  Transposed temporaries are detected by
   looking at their strides.

   The equivalent Fortran pseudo-code is:

   DIMENSION A(M,COUNT), B(COUNT,N), C(M,N)
   IF (.NOT.IS_TRANSPOSED(A)) THEN
     C = 0
     DO J=1,N
       DO K=1,COUNT
         DO I=1,M
           C(I,J) = C(I,J)+A(I,K)*B(K,J)
   ELSE
     DO J=1,N
       DO I=1,M
         S = 0
         DO K=1,COUNT
           S = S+A(I,K)*B(K,J)
         C(I,J) = S
   ENDIF
*/

/* If try_blas is set to a nonzero value, then the matmul function will
   see if there is a way to perform the matrix multiplication by a call
   to the BLAS gemm function.  */

extern void matmul_'rtype_code` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm);
export_proto(matmul_'rtype_code`);

'ifelse(rtype_letter,`r',dnl
`#if defined(HAVE_AVX) && defined(HAVE_AVX2)
/* REAL types generate identical code for AVX and AVX2.  Only generate
   an AVX2 function if we are dealing with integer.  */
#undef HAVE_AVX2
#endif')
`

/* Put exhaustive list of possible architectures here here, ORed together.  */

#if defined(HAVE_AVX) || defined(HAVE_AVX2) || defined(HAVE_AVX512F)

#ifdef HAVE_AVX
'define(`matmul_name',`matmul_'rtype_code`_avx')dnl
`static void
'matmul_name` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx")));
static' include(matmul_internal.m4)dnl
`#endif /* HAVE_AVX */

#ifdef HAVE_AVX2
'define(`matmul_name',`matmul_'rtype_code`_avx2')dnl
`static void
'matmul_name` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx2")));
static' include(matmul_internal.m4)dnl
`#endif /* HAVE_AVX2 */

#ifdef HAVE_AVX512F
'define(`matmul_name',`matmul_'rtype_code`_avx512f')dnl
`static void
'matmul_name` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx512f")));
static' include(matmul_internal.m4)dnl
`#endif  /* HAVE_AVX512F */

/* Function to fall back to if there is no special processor-specific version.  */
'define(`matmul_name',`matmul_'rtype_code`_vanilla')dnl
`static' include(matmul_internal.m4)dnl

`/* Compiling main function, with selection code for the processor.  */

/* Currently, this is i386 only.  Adjust for other architectures.  */

#include <config/i386/cpuinfo.h>
void matmul_'rtype_code` ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm)
{
  static void (*matmul_p) ('rtype` * const restrict retarray, 
	'rtype` * const restrict a, 'rtype` * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) = NULL;

  if (matmul_p == NULL)
    {
      matmul_p = matmul_'rtype_code`_vanilla;
      if (__cpu_model.__cpu_vendor == VENDOR_INTEL)
	{
          /* Run down the available processors in order of preference.  */
#ifdef HAVE_AVX512F
      	  if (__cpu_model.__cpu_features[0] & (1 << FEATURE_AVX512F))
	    {
	      matmul_p = matmul_'rtype_code`_avx512f;
	      goto tailcall;
	    }

#endif  /* HAVE_AVX512F */

#ifdef HAVE_AVX2
      	  if (__cpu_model.__cpu_features[0] & (1 << FEATURE_AVX2))
	    {
	      matmul_p = matmul_'rtype_code`_avx2;
	      goto tailcall;
	    }

#endif

#ifdef HAVE_AVX
      	  if (__cpu_model.__cpu_features[0] & (1 << FEATURE_AVX))
 	    {
              matmul_p = matmul_'rtype_code`_avx;
	      goto tailcall;
	    }
#endif  /* HAVE_AVX */
        }
   }

tailcall:
   (*matmul_p) (retarray, a, b, try_blas, blas_limit, gemm);
}

#else  /* Just the vanilla function.  */

'define(`matmul_name',`matmul_'rtype_code)dnl
define(`target_attribute',`')dnl
include(matmul_internal.m4)dnl
`#endif
#endif
'
