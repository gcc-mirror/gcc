/* Implementation of the ANY intrinsic
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

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

void
__any_l4 (gfc_array_l4 * retarray, gfc_array_l4 *array, index_type *pdim)
{
  index_type count[GFC_MAX_DIMENSIONS - 1];
  index_type extent[GFC_MAX_DIMENSIONS - 1];
  index_type sstride[GFC_MAX_DIMENSIONS - 1];
  index_type dstride[GFC_MAX_DIMENSIONS - 1];
  GFC_LOGICAL_4 *base;
  GFC_LOGICAL_4 *dest;
  index_type rank;
  index_type n;
  index_type len;
  index_type delta;
  index_type dim;

  /* Make dim zero based to avoid confusion.  */
  dim = (*pdim) - 1;
  rank = GFC_DESCRIPTOR_RANK (array) - 1;
  assert (rank == GFC_DESCRIPTOR_RANK (retarray));
  if (array->dim[0].stride == 0)
    array->dim[0].stride = 1;
  if (retarray->dim[0].stride == 0)
    retarray->dim[0].stride = 1;

  len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
  delta = array->dim[dim].stride;

  for (n = 0; n < dim; n++)
    {
      sstride[n] = array->dim[n].stride;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
    }
  for (n = dim; n < rank; n++)
    {
      sstride[n] = array->dim[n + 1].stride;
      extent[n] =
        array->dim[n + 1].ubound + 1 - array->dim[n + 1].lbound;
    }

  if (retarray->data == NULL)
    {
      for (n = 0; n < rank; n++)
        {
          retarray->dim[n].lbound = 0;
          retarray->dim[n].ubound = extent[n]-1;
          if (n == 0)
            retarray->dim[n].stride = 1;
          else
            retarray->dim[n].stride = retarray->dim[n-1].stride * extent[n-1];
        }

      retarray->data = internal_malloc (sizeof (GFC_LOGICAL_4) * 
                                        (retarray->dim[rank-1].stride * extent[rank-1]));
      retarray->base = 0;
    }
          
  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      dstride[n] = retarray->dim[n].stride;
      if (extent[n] <= 0)
        len = 0;
    }

  base = array->data;
  dest = retarray->data;

  while (base)
    {
      GFC_LOGICAL_4 *src;
      GFC_LOGICAL_4 result;
      src = base;
      {

  result = 0;
        if (len <= 0)
	  *dest = 0;
	else
	  {
	    for (n = 0; n < len; n++, src += delta)
	      {

  /* Return true if any of the elements are set.  */
  if (*src)
    {
      result = 1;
      break;
    }
          }
	    *dest = result;
	  }
      }
      /* Advance to the next element.  */
      count[0]++;
      base += sstride[0];
      dest += dstride[0];
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so proabably not worth it.  */
          base -= sstride[n] * extent[n];
          dest -= dstride[n] * extent[n];
          n++;
          if (n == rank)
            {
              /* Break out of the look.  */
              base = NULL;
              break;
            }
          else
            {
              count[n]++;
              base += sstride[n];
              dest += dstride[n];
            }
        }
    }
}

