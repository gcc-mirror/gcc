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
#include <assert.h>
#include "libgfortran.h"

#if defined (HAVE_GFC_LOGICAL_16)

/* Dimensions: retarray(x,y) a(x, count) b(count,y).
   Either a or b can be rank 1.  In this case x or y is 1.  */

extern void matmul_l16 (gfc_array_l16 * const restrict, 
	gfc_array_l4 * const restrict, gfc_array_l4 * const restrict);
export_proto(matmul_l16);

void
matmul_l16 (gfc_array_l16 * const restrict retarray, 
	gfc_array_l4 * const restrict a, gfc_array_l4 * const restrict b)
{
  const GFC_INTEGER_4 * restrict abase;
  const GFC_INTEGER_4 * restrict bbase;
  GFC_LOGICAL_16 * restrict dest;
  index_type rxstride;
  index_type rystride;
  index_type xcount;
  index_type ycount;
  index_type xstride;
  index_type ystride;
  index_type x;
  index_type y;

  const GFC_INTEGER_4 * restrict pa;
  const GFC_INTEGER_4 * restrict pb;
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
          
      retarray->data
	= internal_malloc_size (sizeof (GFC_LOGICAL_16) * size0 ((array_t *) retarray));
      retarray->offset = 0;
    }

  abase = a->data;
  if (GFC_DESCRIPTOR_SIZE (a) != 4)
    {
      assert (GFC_DESCRIPTOR_SIZE (a) == 8);
      abase = GFOR_POINTER_L8_TO_L4 (abase);
    }
  bbase = b->data;
  if (GFC_DESCRIPTOR_SIZE (b) != 4)
    {
      assert (GFC_DESCRIPTOR_SIZE (b) == 8);
      bbase = GFOR_POINTER_L8_TO_L4 (bbase);
    }
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
          *dest = 0;

          for (n = 0; n < count; n++)
            {
              if (*pa && *pb)
                {
                  *dest = 1;
                  break;
                }
              pa += astride;
              pb += bstride;
            }

          dest += rxstride;
          abase += xstride;
        }
      abase -= xstride * xcount;
      bbase += ystride;
      dest += rystride - (rxstride * xcount);
    }
}

#endif
