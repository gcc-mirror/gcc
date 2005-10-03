/* Implementation of the MATMUL intrinsic
   Copyright 2002, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgfortran.h"

#if defined (HAVE_GFC_REAL_16)

/* This is a C version of the following fortran pseudo-code. The key
   point is the loop order -- we access all arrays column-first, which
   improves the performance enough to boost galgel spec score by 50%.

   DIMENSION A(M,COUNT), B(COUNT,N), C(M,N)
   C = 0
   DO J=1,N
     DO K=1,COUNT
       DO I=1,M
         C(I,J) = C(I,J)+A(I,K)*B(K,J)
*/

extern void matmul_r16 (gfc_array_r16 * retarray, gfc_array_r16 * a, gfc_array_r16 * b);
export_proto(matmul_r16);

void
matmul_r16 (gfc_array_r16 * retarray, gfc_array_r16 * a, gfc_array_r16 * b)
{
  GFC_REAL_16 *abase;
  GFC_REAL_16 *bbase;
  GFC_REAL_16 *dest;

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
          retarray->dim[0].lbound = 0;
          retarray->dim[0].ubound = b->dim[1].ubound - b->dim[1].lbound;
          retarray->dim[0].stride = 1;
        }
      else if (GFC_DESCRIPTOR_RANK (b) == 1)
        {
          retarray->dim[0].lbound = 0;
          retarray->dim[0].ubound = a->dim[0].ubound - a->dim[0].lbound;
          retarray->dim[0].stride = 1;
        }
      else
        {
          retarray->dim[0].lbound = 0;
          retarray->dim[0].ubound = a->dim[0].ubound - a->dim[0].lbound;
          retarray->dim[0].stride = 1;

          retarray->dim[1].lbound = 0;
          retarray->dim[1].ubound = b->dim[1].ubound - b->dim[1].lbound;
          retarray->dim[1].stride = retarray->dim[0].ubound+1;
        }

      retarray->data
	= internal_malloc_size (sizeof (GFC_REAL_16) * size0 ((array_t *) retarray));
      retarray->offset = 0;
    }

  abase = a->data;
  bbase = b->data;
  dest = retarray->data;

  if (retarray->dim[0].stride == 0)
    retarray->dim[0].stride = 1;
  if (a->dim[0].stride == 0)
    a->dim[0].stride = 1;
  if (b->dim[0].stride == 0)
    b->dim[0].stride = 1;


  if (GFC_DESCRIPTOR_RANK (retarray) == 1)
    {
      /* One-dimensional result may be addressed in the code below
	 either as a row or a column matrix. We want both cases to
	 work. */
      rxstride = rystride = retarray->dim[0].stride;
    }
  else
    {
      rxstride = retarray->dim[0].stride;
      rystride = retarray->dim[1].stride;
    }


  if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      /* Treat it as a a row matrix A[1,count]. */
      axstride = a->dim[0].stride;
      aystride = 1;

      xcount = 1;
      count = a->dim[0].ubound + 1 - a->dim[0].lbound;
    }
  else
    {
      axstride = a->dim[0].stride;
      aystride = a->dim[1].stride;

      count = a->dim[1].ubound + 1 - a->dim[1].lbound;
      xcount = a->dim[0].ubound + 1 - a->dim[0].lbound;
    }

  assert(count == b->dim[0].ubound + 1 - b->dim[0].lbound);

  if (GFC_DESCRIPTOR_RANK (b) == 1)
    {
      /* Treat it as a column matrix B[count,1] */
      bxstride = b->dim[0].stride;

      /* bystride should never be used for 1-dimensional b.
	 in case it is we want it to cause a segfault, rather than
	 an incorrect result. */
      bystride = 0xDEADBEEF;
      ycount = 1;
    }
  else
    {
      bxstride = b->dim[0].stride;
      bystride = b->dim[1].stride;
      ycount = b->dim[1].ubound + 1 - b->dim[1].lbound;
    }

  abase = a->data;
  bbase = b->data;
  dest = retarray->data;

  if (rxstride == 1 && axstride == 1 && bxstride == 1)
    {
      GFC_REAL_16 *bbase_y;
      GFC_REAL_16 *dest_y;
      GFC_REAL_16 *abase_n;
      GFC_REAL_16 bbase_yn;

      if (rystride == ycount)
	memset (dest, 0, (sizeof (GFC_REAL_16) * size0((array_t *) retarray)));
      else
	{
	  for (y = 0; y < ycount; y++)
	    for (x = 0; x < xcount; x++)
	      dest[x + y*rystride] = (GFC_REAL_16)0;
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
  else
    {
      for (y = 0; y < ycount; y++)
	for (x = 0; x < xcount; x++)
	  dest[x*rxstride + y*rystride] = (GFC_REAL_16)0;

      for (y = 0; y < ycount; y++)
	for (n = 0; n < count; n++)
	  for (x = 0; x < xcount; x++)
	    /* dest[x,y] += a[x,n] * b[n,y] */
	    dest[x*rxstride + y*rystride] += abase[x*axstride + n*aystride] * bbase[n*bxstride + y*bystride];
    }
}

#endif
