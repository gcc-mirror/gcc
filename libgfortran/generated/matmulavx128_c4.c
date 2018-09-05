/* Implementation of the MATMUL intrinsic
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
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
#include <assert.h>


/* These are the specific versions of matmul with -mprefer-avx128.  */

#if defined (HAVE_GFC_COMPLEX_4)

/* Prototype for the BLAS ?gemm subroutine, a pointer to which can be
   passed to us by the front-end, in which case we call it for large
   matrices.  */

typedef void (*blas_call)(const char *, const char *, const int *, const int *,
                          const int *, const GFC_COMPLEX_4 *, const GFC_COMPLEX_4 *,
                          const int *, const GFC_COMPLEX_4 *, const int *,
                          const GFC_COMPLEX_4 *, GFC_COMPLEX_4 *, const int *,
                          int, int);

#if defined(HAVE_AVX) && defined(HAVE_FMA3) && defined(HAVE_AVX128)
void
matmul_c4_avx128_fma3 (gfc_array_c4 * const restrict retarray, 
	gfc_array_c4 * const restrict a, gfc_array_c4 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx,fma")));
internal_proto(matmul_c4_avx128_fma3);
void
matmul_c4_avx128_fma3 (gfc_array_c4 * const restrict retarray, 
	gfc_array_c4 * const restrict a, gfc_array_c4 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm)
{
  const GFC_COMPLEX_4 * restrict abase;
  const GFC_COMPLEX_4 * restrict bbase;
  GFC_COMPLEX_4 * restrict dest;

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

  if (retarray->base_addr == NULL)
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

      retarray->base_addr
	= xmallocarray (size0 ((array_t *) retarray), sizeof (GFC_COMPLEX_4));
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
         The value is only used for calculation of the
         memory by the buffer.  */
      bystride = 256;
      ycount = 1;
    }
  else
    {
      bxstride = GFC_DESCRIPTOR_STRIDE(b,0);
      bystride = GFC_DESCRIPTOR_STRIDE(b,1);
      ycount = GFC_DESCRIPTOR_EXTENT(b,1);
    }

  abase = a->base_addr;
  bbase = b->base_addr;
  dest = retarray->base_addr;

  /* Now that everything is set up, we perform the multiplication
     itself.  */

#define POW3(x) (((float) (x)) * ((float) (x)) * ((float) (x)))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

  if (try_blas && rxstride == 1 && (axstride == 1 || aystride == 1)
      && (bxstride == 1 || bystride == 1)
      && (((float) xcount) * ((float) ycount) * ((float) count)
          > POW3(blas_limit)))
    {
      const int m = xcount, n = ycount, k = count, ldc = rystride;
      const GFC_COMPLEX_4 one = 1, zero = 0;
      const int lda = (axstride == 1) ? aystride : axstride,
		ldb = (bxstride == 1) ? bystride : bxstride;

      if (lda > 0 && ldb > 0 && ldc > 0 && m > 1 && n > 1 && k > 1)
	{
	  assert (gemm != NULL);
	  gemm (axstride == 1 ? "N" : "T", bxstride == 1 ? "N" : "T", &m,
		&n, &k,	&one, abase, &lda, bbase, &ldb, &zero, dest,
		&ldc, 1, 1);
	  return;
	}
    }

  if (rxstride == 1 && axstride == 1 && bxstride == 1)
    {
      /* This block of code implements a tuned matmul, derived from
         Superscalar GEMM-based level 3 BLAS,  Beta version 0.1

               Bo Kagstrom and Per Ling
               Department of Computing Science
               Umea University
               S-901 87 Umea, Sweden

	 from netlib.org, translated to C, and modified for matmul.m4.  */

      const GFC_COMPLEX_4 *a, *b;
      GFC_COMPLEX_4 *c;
      const index_type m = xcount, n = ycount, k = count;

      /* System generated locals */
      index_type a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset,
		 i1, i2, i3, i4, i5, i6;

      /* Local variables */
      GFC_COMPLEX_4 f11, f12, f21, f22, f31, f32, f41, f42,
		 f13, f14, f23, f24, f33, f34, f43, f44;
      index_type i, j, l, ii, jj, ll;
      index_type isec, jsec, lsec, uisec, ujsec, ulsec;
      GFC_COMPLEX_4 *t1;

      a = abase;
      b = bbase;
      c = retarray->base_addr;

      /* Parameter adjustments */
      c_dim1 = rystride;
      c_offset = 1 + c_dim1;
      c -= c_offset;
      a_dim1 = aystride;
      a_offset = 1 + a_dim1;
      a -= a_offset;
      b_dim1 = bystride;
      b_offset = 1 + b_dim1;
      b -= b_offset;

      /* Empty c first.  */
      for (j=1; j<=n; j++)
	for (i=1; i<=m; i++)
	  c[i + j * c_dim1] = (GFC_COMPLEX_4)0;

      /* Early exit if possible */
      if (m == 0 || n == 0 || k == 0)
	return;

      /* Adjust size of t1 to what is needed.  */
      index_type t1_dim, a_sz;
      if (aystride == 1)
        a_sz = rystride;
      else
        a_sz = a_dim1;

      t1_dim = a_sz * 256 + b_dim1;
      if (t1_dim > 65536)
	t1_dim = 65536;

      t1 = malloc (t1_dim * sizeof(GFC_COMPLEX_4));

      /* Start turning the crank. */
      i1 = n;
      for (jj = 1; jj <= i1; jj += 512)
	{
	  /* Computing MIN */
	  i2 = 512;
	  i3 = n - jj + 1;
	  jsec = min(i2,i3);
	  ujsec = jsec - jsec % 4;
	  i2 = k;
	  for (ll = 1; ll <= i2; ll += 256)
	    {
	      /* Computing MIN */
	      i3 = 256;
	      i4 = k - ll + 1;
	      lsec = min(i3,i4);
	      ulsec = lsec - lsec % 2;

	      i3 = m;
	      for (ii = 1; ii <= i3; ii += 256)
		{
		  /* Computing MIN */
		  i4 = 256;
		  i5 = m - ii + 1;
		  isec = min(i4,i5);
		  uisec = isec - isec % 2;
		  i4 = ll + ulsec - 1;
		  for (l = ll; l <= i4; l += 2)
		    {
		      i5 = ii + uisec - 1;
		      for (i = ii; i <= i5; i += 2)
			{
			  t1[l - ll + 1 + ((i - ii + 1) << 8) - 257] =
					a[i + l * a_dim1];
			  t1[l - ll + 2 + ((i - ii + 1) << 8) - 257] =
					a[i + (l + 1) * a_dim1];
			  t1[l - ll + 1 + ((i - ii + 2) << 8) - 257] =
					a[i + 1 + l * a_dim1];
			  t1[l - ll + 2 + ((i - ii + 2) << 8) - 257] =
					a[i + 1 + (l + 1) * a_dim1];
			}
		      if (uisec < isec)
			{
			  t1[l - ll + 1 + (isec << 8) - 257] =
				    a[ii + isec - 1 + l * a_dim1];
			  t1[l - ll + 2 + (isec << 8) - 257] =
				    a[ii + isec - 1 + (l + 1) * a_dim1];
			}
		    }
		  if (ulsec < lsec)
		    {
		      i4 = ii + isec - 1;
		      for (i = ii; i<= i4; ++i)
			{
			  t1[lsec + ((i - ii + 1) << 8) - 257] =
				    a[i + (ll + lsec - 1) * a_dim1];
			}
		    }

		  uisec = isec - isec % 4;
		  i4 = jj + ujsec - 1;
		  for (j = jj; j <= i4; j += 4)
		    {
		      i5 = ii + uisec - 1;
		      for (i = ii; i <= i5; i += 4)
			{
			  f11 = c[i + j * c_dim1];
			  f21 = c[i + 1 + j * c_dim1];
			  f12 = c[i + (j + 1) * c_dim1];
			  f22 = c[i + 1 + (j + 1) * c_dim1];
			  f13 = c[i + (j + 2) * c_dim1];
			  f23 = c[i + 1 + (j + 2) * c_dim1];
			  f14 = c[i + (j + 3) * c_dim1];
			  f24 = c[i + 1 + (j + 3) * c_dim1];
			  f31 = c[i + 2 + j * c_dim1];
			  f41 = c[i + 3 + j * c_dim1];
			  f32 = c[i + 2 + (j + 1) * c_dim1];
			  f42 = c[i + 3 + (j + 1) * c_dim1];
			  f33 = c[i + 2 + (j + 2) * c_dim1];
			  f43 = c[i + 3 + (j + 2) * c_dim1];
			  f34 = c[i + 2 + (j + 3) * c_dim1];
			  f44 = c[i + 3 + (j + 3) * c_dim1];
			  i6 = ll + lsec - 1;
			  for (l = ll; l <= i6; ++l)
			    {
			      f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + j * b_dim1];
			      f21 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + j * b_dim1];
			      f12 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f22 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f13 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f23 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f14 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f24 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f31 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + j * b_dim1];
			      f41 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + j * b_dim1];
			      f32 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f42 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f33 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f43 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f34 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f44 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			    }
			  c[i + j * c_dim1] = f11;
			  c[i + 1 + j * c_dim1] = f21;
			  c[i + (j + 1) * c_dim1] = f12;
			  c[i + 1 + (j + 1) * c_dim1] = f22;
			  c[i + (j + 2) * c_dim1] = f13;
			  c[i + 1 + (j + 2) * c_dim1] = f23;
			  c[i + (j + 3) * c_dim1] = f14;
			  c[i + 1 + (j + 3) * c_dim1] = f24;
			  c[i + 2 + j * c_dim1] = f31;
			  c[i + 3 + j * c_dim1] = f41;
			  c[i + 2 + (j + 1) * c_dim1] = f32;
			  c[i + 3 + (j + 1) * c_dim1] = f42;
			  c[i + 2 + (j + 2) * c_dim1] = f33;
			  c[i + 3 + (j + 2) * c_dim1] = f43;
			  c[i + 2 + (j + 3) * c_dim1] = f34;
			  c[i + 3 + (j + 3) * c_dim1] = f44;
			}
		      if (uisec < isec)
			{
			  i5 = ii + isec - 1;
			  for (i = ii + uisec; i <= i5; ++i)
			    {
			      f11 = c[i + j * c_dim1];
			      f12 = c[i + (j + 1) * c_dim1];
			      f13 = c[i + (j + 2) * c_dim1];
			      f14 = c[i + (j + 3) * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				  f12 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 1) * b_dim1];
				  f13 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 2) * b_dim1];
				  f14 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 3) * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			      c[i + (j + 1) * c_dim1] = f12;
			      c[i + (j + 2) * c_dim1] = f13;
			      c[i + (j + 3) * c_dim1] = f14;
			    }
			}
		    }
		  if (ujsec < jsec)
		    {
		      i4 = jj + jsec - 1;
		      for (j = jj + ujsec; j <= i4; ++j)
			{
			  i5 = ii + uisec - 1;
			  for (i = ii; i <= i5; i += 4)
			    {
			      f11 = c[i + j * c_dim1];
			      f21 = c[i + 1 + j * c_dim1];
			      f31 = c[i + 2 + j * c_dim1];
			      f41 = c[i + 3 + j * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				  f21 += t1[l - ll + 1 + ((i - ii + 2) << 8) -
					  257] * b[l + j * b_dim1];
				  f31 += t1[l - ll + 1 + ((i - ii + 3) << 8) -
					  257] * b[l + j * b_dim1];
				  f41 += t1[l - ll + 1 + ((i - ii + 4) << 8) -
					  257] * b[l + j * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			      c[i + 1 + j * c_dim1] = f21;
			      c[i + 2 + j * c_dim1] = f31;
			      c[i + 3 + j * c_dim1] = f41;
			    }
			  i5 = ii + isec - 1;
			  for (i = ii + uisec; i <= i5; ++i)
			    {
			      f11 = c[i + j * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			    }
			}
		    }
		}
	    }
	}
      free(t1);
      return;
    }
  else if (rxstride == 1 && aystride == 1 && bxstride == 1)
    {
      if (GFC_DESCRIPTOR_RANK (a) != 1)
	{
	  const GFC_COMPLEX_4 *restrict abase_x;
	  const GFC_COMPLEX_4 *restrict bbase_y;
	  GFC_COMPLEX_4 *restrict dest_y;
	  GFC_COMPLEX_4 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      dest_y = &dest[y*rystride];
	      for (x = 0; x < xcount; x++)
		{
		  abase_x = &abase[x*axstride];
		  s = (GFC_COMPLEX_4) 0;
		  for (n = 0; n < count; n++)
		    s += abase_x[n] * bbase_y[n];
		  dest_y[x] = s;
		}
	    }
	}
      else
	{
	  const GFC_COMPLEX_4 *restrict bbase_y;
	  GFC_COMPLEX_4 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      s = (GFC_COMPLEX_4) 0;
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
	  dest[x*rxstride + y*rystride] = (GFC_COMPLEX_4)0;

      for (y = 0; y < ycount; y++)
	for (n = 0; n < count; n++)
	  for (x = 0; x < xcount; x++)
	    /* dest[x,y] += a[x,n] * b[n,y] */
	    dest[x*rxstride + y*rystride] +=
					abase[x*axstride + n*aystride] *
					bbase[n*bxstride + y*bystride];
    }
  else if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      const GFC_COMPLEX_4 *restrict bbase_y;
      GFC_COMPLEX_4 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  s = (GFC_COMPLEX_4) 0;
	  for (n = 0; n < count; n++)
	    s += abase[n*axstride] * bbase_y[n*bxstride];
	  dest[y*rxstride] = s;
	}
    }
  else
    {
      const GFC_COMPLEX_4 *restrict abase_x;
      const GFC_COMPLEX_4 *restrict bbase_y;
      GFC_COMPLEX_4 *restrict dest_y;
      GFC_COMPLEX_4 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  dest_y = &dest[y*rystride];
	  for (x = 0; x < xcount; x++)
	    {
	      abase_x = &abase[x*axstride];
	      s = (GFC_COMPLEX_4) 0;
	      for (n = 0; n < count; n++)
		s += abase_x[n*aystride] * bbase_y[n*bxstride];
	      dest_y[x*rxstride] = s;
	    }
	}
    }
}
#undef POW3
#undef min
#undef max

#endif

#if defined(HAVE_AVX) && defined(HAVE_FMA4) && defined(HAVE_AVX128)
void
matmul_c4_avx128_fma4 (gfc_array_c4 * const restrict retarray, 
	gfc_array_c4 * const restrict a, gfc_array_c4 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm) __attribute__((__target__("avx,fma4")));
internal_proto(matmul_c4_avx128_fma4);
void
matmul_c4_avx128_fma4 (gfc_array_c4 * const restrict retarray, 
	gfc_array_c4 * const restrict a, gfc_array_c4 * const restrict b, int try_blas,
	int blas_limit, blas_call gemm)
{
  const GFC_COMPLEX_4 * restrict abase;
  const GFC_COMPLEX_4 * restrict bbase;
  GFC_COMPLEX_4 * restrict dest;

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

  if (retarray->base_addr == NULL)
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

      retarray->base_addr
	= xmallocarray (size0 ((array_t *) retarray), sizeof (GFC_COMPLEX_4));
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
         The value is only used for calculation of the
         memory by the buffer.  */
      bystride = 256;
      ycount = 1;
    }
  else
    {
      bxstride = GFC_DESCRIPTOR_STRIDE(b,0);
      bystride = GFC_DESCRIPTOR_STRIDE(b,1);
      ycount = GFC_DESCRIPTOR_EXTENT(b,1);
    }

  abase = a->base_addr;
  bbase = b->base_addr;
  dest = retarray->base_addr;

  /* Now that everything is set up, we perform the multiplication
     itself.  */

#define POW3(x) (((float) (x)) * ((float) (x)) * ((float) (x)))
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

  if (try_blas && rxstride == 1 && (axstride == 1 || aystride == 1)
      && (bxstride == 1 || bystride == 1)
      && (((float) xcount) * ((float) ycount) * ((float) count)
          > POW3(blas_limit)))
    {
      const int m = xcount, n = ycount, k = count, ldc = rystride;
      const GFC_COMPLEX_4 one = 1, zero = 0;
      const int lda = (axstride == 1) ? aystride : axstride,
		ldb = (bxstride == 1) ? bystride : bxstride;

      if (lda > 0 && ldb > 0 && ldc > 0 && m > 1 && n > 1 && k > 1)
	{
	  assert (gemm != NULL);
	  gemm (axstride == 1 ? "N" : "T", bxstride == 1 ? "N" : "T", &m,
		&n, &k,	&one, abase, &lda, bbase, &ldb, &zero, dest,
		&ldc, 1, 1);
	  return;
	}
    }

  if (rxstride == 1 && axstride == 1 && bxstride == 1)
    {
      /* This block of code implements a tuned matmul, derived from
         Superscalar GEMM-based level 3 BLAS,  Beta version 0.1

               Bo Kagstrom and Per Ling
               Department of Computing Science
               Umea University
               S-901 87 Umea, Sweden

	 from netlib.org, translated to C, and modified for matmul.m4.  */

      const GFC_COMPLEX_4 *a, *b;
      GFC_COMPLEX_4 *c;
      const index_type m = xcount, n = ycount, k = count;

      /* System generated locals */
      index_type a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset,
		 i1, i2, i3, i4, i5, i6;

      /* Local variables */
      GFC_COMPLEX_4 f11, f12, f21, f22, f31, f32, f41, f42,
		 f13, f14, f23, f24, f33, f34, f43, f44;
      index_type i, j, l, ii, jj, ll;
      index_type isec, jsec, lsec, uisec, ujsec, ulsec;
      GFC_COMPLEX_4 *t1;

      a = abase;
      b = bbase;
      c = retarray->base_addr;

      /* Parameter adjustments */
      c_dim1 = rystride;
      c_offset = 1 + c_dim1;
      c -= c_offset;
      a_dim1 = aystride;
      a_offset = 1 + a_dim1;
      a -= a_offset;
      b_dim1 = bystride;
      b_offset = 1 + b_dim1;
      b -= b_offset;

      /* Empty c first.  */
      for (j=1; j<=n; j++)
	for (i=1; i<=m; i++)
	  c[i + j * c_dim1] = (GFC_COMPLEX_4)0;

      /* Early exit if possible */
      if (m == 0 || n == 0 || k == 0)
	return;

      /* Adjust size of t1 to what is needed.  */
      index_type t1_dim, a_sz;
      if (aystride == 1)
        a_sz = rystride;
      else
        a_sz = a_dim1;

      t1_dim = a_sz * 256 + b_dim1;
      if (t1_dim > 65536)
	t1_dim = 65536;

      t1 = malloc (t1_dim * sizeof(GFC_COMPLEX_4));

      /* Start turning the crank. */
      i1 = n;
      for (jj = 1; jj <= i1; jj += 512)
	{
	  /* Computing MIN */
	  i2 = 512;
	  i3 = n - jj + 1;
	  jsec = min(i2,i3);
	  ujsec = jsec - jsec % 4;
	  i2 = k;
	  for (ll = 1; ll <= i2; ll += 256)
	    {
	      /* Computing MIN */
	      i3 = 256;
	      i4 = k - ll + 1;
	      lsec = min(i3,i4);
	      ulsec = lsec - lsec % 2;

	      i3 = m;
	      for (ii = 1; ii <= i3; ii += 256)
		{
		  /* Computing MIN */
		  i4 = 256;
		  i5 = m - ii + 1;
		  isec = min(i4,i5);
		  uisec = isec - isec % 2;
		  i4 = ll + ulsec - 1;
		  for (l = ll; l <= i4; l += 2)
		    {
		      i5 = ii + uisec - 1;
		      for (i = ii; i <= i5; i += 2)
			{
			  t1[l - ll + 1 + ((i - ii + 1) << 8) - 257] =
					a[i + l * a_dim1];
			  t1[l - ll + 2 + ((i - ii + 1) << 8) - 257] =
					a[i + (l + 1) * a_dim1];
			  t1[l - ll + 1 + ((i - ii + 2) << 8) - 257] =
					a[i + 1 + l * a_dim1];
			  t1[l - ll + 2 + ((i - ii + 2) << 8) - 257] =
					a[i + 1 + (l + 1) * a_dim1];
			}
		      if (uisec < isec)
			{
			  t1[l - ll + 1 + (isec << 8) - 257] =
				    a[ii + isec - 1 + l * a_dim1];
			  t1[l - ll + 2 + (isec << 8) - 257] =
				    a[ii + isec - 1 + (l + 1) * a_dim1];
			}
		    }
		  if (ulsec < lsec)
		    {
		      i4 = ii + isec - 1;
		      for (i = ii; i<= i4; ++i)
			{
			  t1[lsec + ((i - ii + 1) << 8) - 257] =
				    a[i + (ll + lsec - 1) * a_dim1];
			}
		    }

		  uisec = isec - isec % 4;
		  i4 = jj + ujsec - 1;
		  for (j = jj; j <= i4; j += 4)
		    {
		      i5 = ii + uisec - 1;
		      for (i = ii; i <= i5; i += 4)
			{
			  f11 = c[i + j * c_dim1];
			  f21 = c[i + 1 + j * c_dim1];
			  f12 = c[i + (j + 1) * c_dim1];
			  f22 = c[i + 1 + (j + 1) * c_dim1];
			  f13 = c[i + (j + 2) * c_dim1];
			  f23 = c[i + 1 + (j + 2) * c_dim1];
			  f14 = c[i + (j + 3) * c_dim1];
			  f24 = c[i + 1 + (j + 3) * c_dim1];
			  f31 = c[i + 2 + j * c_dim1];
			  f41 = c[i + 3 + j * c_dim1];
			  f32 = c[i + 2 + (j + 1) * c_dim1];
			  f42 = c[i + 3 + (j + 1) * c_dim1];
			  f33 = c[i + 2 + (j + 2) * c_dim1];
			  f43 = c[i + 3 + (j + 2) * c_dim1];
			  f34 = c[i + 2 + (j + 3) * c_dim1];
			  f44 = c[i + 3 + (j + 3) * c_dim1];
			  i6 = ll + lsec - 1;
			  for (l = ll; l <= i6; ++l)
			    {
			      f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + j * b_dim1];
			      f21 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + j * b_dim1];
			      f12 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f22 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f13 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f23 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f14 += t1[l - ll + 1 + ((i - ii + 1) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f24 += t1[l - ll + 1 + ((i - ii + 2) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f31 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + j * b_dim1];
			      f41 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + j * b_dim1];
			      f32 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f42 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 1) * b_dim1];
			      f33 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f43 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 2) * b_dim1];
			      f34 += t1[l - ll + 1 + ((i - ii + 3) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			      f44 += t1[l - ll + 1 + ((i - ii + 4) << 8) - 257]
				      * b[l + (j + 3) * b_dim1];
			    }
			  c[i + j * c_dim1] = f11;
			  c[i + 1 + j * c_dim1] = f21;
			  c[i + (j + 1) * c_dim1] = f12;
			  c[i + 1 + (j + 1) * c_dim1] = f22;
			  c[i + (j + 2) * c_dim1] = f13;
			  c[i + 1 + (j + 2) * c_dim1] = f23;
			  c[i + (j + 3) * c_dim1] = f14;
			  c[i + 1 + (j + 3) * c_dim1] = f24;
			  c[i + 2 + j * c_dim1] = f31;
			  c[i + 3 + j * c_dim1] = f41;
			  c[i + 2 + (j + 1) * c_dim1] = f32;
			  c[i + 3 + (j + 1) * c_dim1] = f42;
			  c[i + 2 + (j + 2) * c_dim1] = f33;
			  c[i + 3 + (j + 2) * c_dim1] = f43;
			  c[i + 2 + (j + 3) * c_dim1] = f34;
			  c[i + 3 + (j + 3) * c_dim1] = f44;
			}
		      if (uisec < isec)
			{
			  i5 = ii + isec - 1;
			  for (i = ii + uisec; i <= i5; ++i)
			    {
			      f11 = c[i + j * c_dim1];
			      f12 = c[i + (j + 1) * c_dim1];
			      f13 = c[i + (j + 2) * c_dim1];
			      f14 = c[i + (j + 3) * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				  f12 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 1) * b_dim1];
				  f13 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 2) * b_dim1];
				  f14 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + (j + 3) * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			      c[i + (j + 1) * c_dim1] = f12;
			      c[i + (j + 2) * c_dim1] = f13;
			      c[i + (j + 3) * c_dim1] = f14;
			    }
			}
		    }
		  if (ujsec < jsec)
		    {
		      i4 = jj + jsec - 1;
		      for (j = jj + ujsec; j <= i4; ++j)
			{
			  i5 = ii + uisec - 1;
			  for (i = ii; i <= i5; i += 4)
			    {
			      f11 = c[i + j * c_dim1];
			      f21 = c[i + 1 + j * c_dim1];
			      f31 = c[i + 2 + j * c_dim1];
			      f41 = c[i + 3 + j * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				  f21 += t1[l - ll + 1 + ((i - ii + 2) << 8) -
					  257] * b[l + j * b_dim1];
				  f31 += t1[l - ll + 1 + ((i - ii + 3) << 8) -
					  257] * b[l + j * b_dim1];
				  f41 += t1[l - ll + 1 + ((i - ii + 4) << 8) -
					  257] * b[l + j * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			      c[i + 1 + j * c_dim1] = f21;
			      c[i + 2 + j * c_dim1] = f31;
			      c[i + 3 + j * c_dim1] = f41;
			    }
			  i5 = ii + isec - 1;
			  for (i = ii + uisec; i <= i5; ++i)
			    {
			      f11 = c[i + j * c_dim1];
			      i6 = ll + lsec - 1;
			      for (l = ll; l <= i6; ++l)
				{
				  f11 += t1[l - ll + 1 + ((i - ii + 1) << 8) -
					  257] * b[l + j * b_dim1];
				}
			      c[i + j * c_dim1] = f11;
			    }
			}
		    }
		}
	    }
	}
      free(t1);
      return;
    }
  else if (rxstride == 1 && aystride == 1 && bxstride == 1)
    {
      if (GFC_DESCRIPTOR_RANK (a) != 1)
	{
	  const GFC_COMPLEX_4 *restrict abase_x;
	  const GFC_COMPLEX_4 *restrict bbase_y;
	  GFC_COMPLEX_4 *restrict dest_y;
	  GFC_COMPLEX_4 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      dest_y = &dest[y*rystride];
	      for (x = 0; x < xcount; x++)
		{
		  abase_x = &abase[x*axstride];
		  s = (GFC_COMPLEX_4) 0;
		  for (n = 0; n < count; n++)
		    s += abase_x[n] * bbase_y[n];
		  dest_y[x] = s;
		}
	    }
	}
      else
	{
	  const GFC_COMPLEX_4 *restrict bbase_y;
	  GFC_COMPLEX_4 s;

	  for (y = 0; y < ycount; y++)
	    {
	      bbase_y = &bbase[y*bystride];
	      s = (GFC_COMPLEX_4) 0;
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
	  dest[x*rxstride + y*rystride] = (GFC_COMPLEX_4)0;

      for (y = 0; y < ycount; y++)
	for (n = 0; n < count; n++)
	  for (x = 0; x < xcount; x++)
	    /* dest[x,y] += a[x,n] * b[n,y] */
	    dest[x*rxstride + y*rystride] +=
					abase[x*axstride + n*aystride] *
					bbase[n*bxstride + y*bystride];
    }
  else if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      const GFC_COMPLEX_4 *restrict bbase_y;
      GFC_COMPLEX_4 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  s = (GFC_COMPLEX_4) 0;
	  for (n = 0; n < count; n++)
	    s += abase[n*axstride] * bbase_y[n*bxstride];
	  dest[y*rxstride] = s;
	}
    }
  else
    {
      const GFC_COMPLEX_4 *restrict abase_x;
      const GFC_COMPLEX_4 *restrict bbase_y;
      GFC_COMPLEX_4 *restrict dest_y;
      GFC_COMPLEX_4 s;

      for (y = 0; y < ycount; y++)
	{
	  bbase_y = &bbase[y*bystride];
	  dest_y = &dest[y*rystride];
	  for (x = 0; x < xcount; x++)
	    {
	      abase_x = &abase[x*axstride];
	      s = (GFC_COMPLEX_4) 0;
	      for (n = 0; n < count; n++)
		s += abase_x[n*aystride] * bbase_y[n*bxstride];
	      dest_y[x*rxstride] = s;
	    }
	}
    }
}
#undef POW3
#undef min
#undef max

#endif

#endif

