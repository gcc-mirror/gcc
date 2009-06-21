/* Implementation of the MATMUL intrinsic
   Copyright 2002, 2005, 2006, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <assert.h>


#if defined (HAVE_GFC_INTEGER_2)

/* Prototype for the BLAS ?gemm subroutine, a pointer to which can be
   passed to us by the front-end, in which case we'll call it for large
   matrices.  */

typedef void (*blas_call)(const char *, const char *, const int *, const int *,
                          const int *, const GFC_INTEGER_2 *, const GFC_INTEGER_2 *,
                          const int *, const GFC_INTEGER_2 *, const int *,
                          const GFC_INTEGER_2 *, GFC_INTEGER_2 *, const int *,
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

extern void matmul_i2 (gfc_array_i2 * const restrict retarray, 
	gfc_array_i2 * const restrict a, gfc_array_i2 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm);
export_proto(matmul_i2);

void
matmul_i2 (gfc_array_i2 * const restrict retarray, 
	gfc_array_i2 * const restrict a, gfc_array_i2 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm)
{
  const GFC_INTEGER_2 * restrict abase;
  const GFC_INTEGER_2 * restrict bbase;
  GFC_INTEGER_2 * restrict dest;

  index_type rxstride, rystride, axstride, aystride, bxstride, bystride;
  index_type x, y, n, count, xcount, ycount;

  assert (GFC_DESCRIPTOR_RANK (a) == 2
          || GFC_DESCRIPTOR_RANK (b) == 2);

/* C[xcount,ycount] = A[xcount, count] * B[count,ycount]

   Either A or B (but not both) can be rank 1:

   o One-dimensional argument A is implicitly treated as a row matrix
     dimensioned [1,count], so xcount=1.

   o One-dimensional argument B is implicitly treated as a column matrix
     dimensioned [count, 1], so ycount=1.
  */

  if (retarray->data == NULL)
    {
      if (GFC_DESCRIPTOR_RANK (a) == 1)
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(b,1) - 1, 1);
        }
      else if (GFC_DESCRIPTOR_RANK (b) == 1)
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(a,0) - 1, 1);
        }
      else
        {
	  GFC_DIMENSION_SET(retarray->dim[0], 0,
	                    GFC_DESCRIPTOR_EXTENT(a,0) - 1, 1);

          GFC_DIMENSION_SET(retarray->dim[1], 0,
	                    GFC_DESCRIPTOR_EXTENT(b,1) - 1,
			    GFC_DESCRIPTOR_EXTENT(retarray,0));
        }

      retarray->data
	= internal_malloc_size (sizeof (GFC_INTEGER_2) * size0 ((array_t *) retarray));
      retarray->offset = 0;
    }
    else if (unlikely (compile_options.bounds_check))
      {
	index_type ret_extent, arg_extent;

	if (GFC_DESCRIPTOR_RANK (a) == 1)
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(b,1);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic: is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);
	  }
	else if (GFC_DESCRIPTOR_RANK (b) == 1)
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(a,0);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic: is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);	    
	  }
	else
	  {
	    arg_extent = GFC_DESCRIPTOR_EXTENT(a,0);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,0);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic for dimension 1:"
			     " is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);

	    arg_extent = GFC_DESCRIPTOR_EXTENT(b,1);
	    ret_extent = GFC_DESCRIPTOR_EXTENT(retarray,1);
	    if (arg_extent != ret_extent)
	      runtime_error ("Incorrect extent in return array in"
			     " MATMUL intrinsic for dimension 2:"
			     " is %ld, should be %ld",
			     (long int) ret_extent, (long int) arg_extent);
	  }
      }


  if (GFC_DESCRIPTOR_RANK (retarray) == 1)
    {
      /* One-dimensional result may be addressed in the code below
	 either as a row or a column matrix. We want both cases to
	 work. */
      rxstride = rystride = GFC_DESCRIPTOR_STRIDE(retarray,0);
    }
  else
    {
      rxstride = GFC_DESCRIPTOR_STRIDE(retarray,0);
      rystride = GFC_DESCRIPTOR_STRIDE(retarray,1);
    }


  if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      /* Treat it as a a row matrix A[1,count]. */
      axstride = GFC_DESCRIPTOR_STRIDE(a,0);
      aystride = 1;

      xcount = 1;
      count = GFC_DESCRIPTOR_EXTENT(a,0);
    }
  else
    {
      axstride = GFC_DESCRIPTOR_STRIDE(a,0);
      aystride = GFC_DESCRIPTOR_STRIDE(a,1);

      count = GFC_DESCRIPTOR_EXTENT(a,1);
      xcount = GFC_DESCRIPTOR_EXTENT(a,0);
    }

  if (count != GFC_DESCRIPTOR_EXTENT(b,0))
    {
      if (count > 0 || GFC_DESCRIPTOR_EXTENT(b,0) > 0)
	runtime_error ("dimension of array B incorrect in MATMUL intrinsic");
    }

  if (GFC_DESCRIPTOR_RANK (b) == 1)
    {
      /* Treat it as a column matrix B[count,1] */
      bxstride = GFC_DESCRIPTOR_STRIDE(b,0);

      /* bystride should never be used for 1-dimensional b.
	 in case it is we want it to cause a segfault, rather than
	 an incorrect result. */
      bystride = 0xDEADBEEF;
      ycount = 1;
    }
  else
    {
      bxstride = GFC_DESCRIPTOR_STRIDE(b,0);
      bystride = GFC_DESCRIPTOR_STRIDE(b,1);
      ycount = GFC_DESCRIPTOR_EXTENT(b,1);
    }

  abase = a->data;
  bbase = b->data;
  dest = retarray->data;


  /* Now that everything is set up, we're performing the multiplication
     itself.  */

#define POW3(x) (((float) (x)) * ((float) (x)) * ((float) (x)))

  if (try_blas && rxstride == 1 && (axstride == 1 || aystride == 1)
      && (bxstride == 1 || bystride == 1)
      && (((float) xcount) * ((float) ycount) * ((float) count)
          > POW3(blas_limit)))
  {
    const int m = xcount, n = ycount, k = count, ldc = rystride;
    const GFC_INTEGER_2 one = 1, zero = 0;
    const int lda = (axstride == 1) ? aystride : axstride,
              ldb = (bxstride == 1) ? bystride : bxstride;

    if (lda > 0 && ldb > 0 && ldc > 0 && m > 1 && n > 1 && k > 1)
      {
        assert (gemm != NULL);
        gemm (axstride == 1 ? "N" : "T", bxstride == 1 ? "N" : "T", &m, &n, &k,
              &one, abase, &lda, bbase, &ldb, &zero, dest, &ldc, 1, 1);
        return;
      }
  }

  if (rxstride == 1 && axstride == 1 && bxstride == 1)
    {
      const GFC_INTEGER_2 * restrict bbase_y;
      GFC_INTEGER_2 * restrict dest_y;
      const GFC_INTEGER_2 * restrict abase_n;
      GFC_INTEGER_2 bbase_yn;

      if (rystride == xcount)
	memset (dest, 0, (sizeof (GFC_INTEGER_2) * xcount * ycount));
      else
	{
	  for (y = 0; y < ycount; y++)
	    for (x = 0; x < xcount; x++)
	      dest[x + y*rystride] = (GFC_INTEGER_2)0;
	}

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = bbase + y*bystride;
	  dest_y = dest + y*rystride;
	  for (n = 0; n < count; n++)
	    {
	      abase_n = abase + n*aystride;
	      bbase_yn = bbase_y[n];
	      for (x = 0; x < xcount; x++)
		{
		  dest_y[x] += abase_n[x] * bbase_yn;
		}
	    }
	}
    }
  else if (rxstride == 1 && aystride == 1 && bxstride == 1)
    {
      if (GFC_DESCRIPTOR_RANK (a) != 1)
	{
	  const GFC_INTEGER_2 *restrict abase_x;
	  const GFC_INTEGER_2 *restrict bbase_y;
	  GFC_INTEGER_2 *restrict dest_y;
	  GFC_INTEGER_2 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      dest_y = &dest[y*rystride];
	      for (x = 0; x < xcount; x++)
		{
		  abase_x = &abase[x*axstride];
		  s = (GFC_INTEGER_2) 0;
		  for (n = 0; n < count; n++)
		    s += abase_x[n] * bbase_y[n];
		  dest_y[x] = s;
		}
	    }
	}
      else
	{
	  const GFC_INTEGER_2 *restrict bbase_y;
	  GFC_INTEGER_2 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      s = (GFC_INTEGER_2) 0;
	      for (n = 0; n < count; n++)
		s += abase[n*axstride] * bbase_y[n];
	      dest[y*rystride] = s;
	    }
	}
    }
  else if (axstride < aystride)
    {
      for (y = 0; y < ycount; y++)
	for (x = 0; x < xcount; x++)
	  dest[x*rxstride + y*rystride] = (GFC_INTEGER_2)0;

      for (y = 0; y < ycount; y++)
	for (n = 0; n < count; n++)
	  for (x = 0; x < xcount; x++)
	    /* dest[x,y] += a[x,n] * b[n,y] */
	    dest[x*rxstride + y*rystride] += abase[x*axstride + n*aystride] * bbase[n*bxstride + y*bystride];
    }
  else if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      const GFC_INTEGER_2 *restrict bbase_y;
      GFC_INTEGER_2 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  s = (GFC_INTEGER_2) 0;
	  for (n = 0; n < count; n++)
	    s += abase[n*axstride] * bbase_y[n*bxstride];
	  dest[y*rxstride] = s;
	}
    }
  else
    {
      const GFC_INTEGER_2 *restrict abase_x;
      const GFC_INTEGER_2 *restrict bbase_y;
      GFC_INTEGER_2 *restrict dest_y;
      GFC_INTEGER_2 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  dest_y = &dest[y*rystride];
	  for (x = 0; x < xcount; x++)
	    {
	      abase_x = &abase[x*axstride];
	      s = (GFC_INTEGER_2) 0;
	      for (n = 0; n < count; n++)
		s += abase_x[n*aystride] * bbase_y[n*bxstride];
	      dest_y[x*rxstride] = s;
	    }
	}
    }
}

#endif
