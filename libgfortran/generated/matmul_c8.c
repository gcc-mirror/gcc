/* Implementation of the MATMUL intrinsic
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <stdlib.h>
#include <assert.h>
#include "libgfortran.h"

/* Dimensions: retarray(x,y) a(x, count) b(count,y).
   Either a or b can be rank 1.  In this case x or y is 1.  */
void
__matmul_c8 (gfc_array_c8 * retarray, gfc_array_c8 * a, gfc_array_c8 * b)
{
  GFC_COMPLEX_8 *abase;
  GFC_COMPLEX_8 *bbase;
  GFC_COMPLEX_8 *dest;
  GFC_COMPLEX_8 res;
  index_type rxstride;
  index_type rystride;
  index_type xcount;
  index_type ycount;
  index_type xstride;
  index_type ystride;
  index_type x;
  index_type y;

  GFC_COMPLEX_8 *pa;
  GFC_COMPLEX_8 *pb;
  index_type astride;
  index_type bstride;
  index_type count;
  index_type n;

  assert (GFC_DESCRIPTOR_RANK (a) == 2
          || GFC_DESCRIPTOR_RANK (b) == 2);

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
          
      retarray->data = internal_malloc (sizeof (GFC_COMPLEX_8) * size0 (retarray));
      retarray->base = 0;
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
      rxstride = retarray->dim[0].stride;
      rystride = rxstride;
    }
  else
    {
      rxstride = retarray->dim[0].stride;
      rystride = retarray->dim[1].stride;
    }

  /* If we have rank 1 parameters, zero the absent stride, and set the size to
     one.  */
  if (GFC_DESCRIPTOR_RANK (a) == 1)
    {
      astride = a->dim[0].stride;
      count = a->dim[0].ubound + 1 - a->dim[0].lbound;
      xstride = 0;
      rxstride = 0;
      xcount = 1;
    }
  else
    {
      astride = a->dim[1].stride;
      count = a->dim[1].ubound + 1 - a->dim[1].lbound;
      xstride = a->dim[0].stride;
      xcount = a->dim[0].ubound + 1 - a->dim[0].lbound;
    }
  if (GFC_DESCRIPTOR_RANK (b) == 1)
    {
      bstride = b->dim[0].stride;
      assert(count == b->dim[0].ubound + 1 - b->dim[0].lbound);
      ystride = 0;
      rystride = 0;
      ycount = 1;
    }
  else
    {
      bstride = b->dim[0].stride;
      assert(count == b->dim[0].ubound + 1 - b->dim[0].lbound);
      ystride = b->dim[1].stride;
      ycount = b->dim[1].ubound + 1 - b->dim[1].lbound;
    }

  for (y = 0; y < ycount; y++)
    {
      for (x = 0; x < xcount; x++)
        {
          /* Do the summation for this element.  For real and integer types
             this is the same as DOT_PRODUCT.  For complex types we use do
             a*b, not conjg(a)*b.  */
          pa = abase;
          pb = bbase;
          res = 0;

          for (n = 0; n < count; n++)
            {
              res += *pa * *pb;
              pa += astride;
              pb += bstride;
            }

          *dest = res;

          dest += rxstride;
          abase += xstride;
        }
      abase -= xstride * xcount;
      bbase += ystride;
      dest += rystride - (rxstride * xcount);
    }
}

