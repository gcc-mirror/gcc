/* Generic implementation of the RESHAPE intrinsic
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Ligbfor is distributed in the hope that it will be useful,
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
#include <string.h>
#include "libgfortran.h"

void
__pack (const gfc_array_char * ret, const gfc_array_char * array,
    const gfc_array_l4 * mask, const gfc_array_char * vector)
{
  /* r.* indicates the return array.  */
  index_type rstride0;
  char *rptr;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  const char *sptr;
  /* m.* indicates the mask array.  */
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type mstride0;
  const GFC_LOGICAL_4 *mptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type n;
  index_type dim;
  index_type size;
  index_type nelem;

  size = GFC_DESCRIPTOR_SIZE (array);
  dim = GFC_DESCRIPTOR_RANK (array);
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      extent[n] = array->dim[n].ubound + 1 - array->dim[n].lbound;
      sstride[n] = array->dim[n].stride * size;
      mstride[n] = mask->dim[n].stride;
    }
  if (sstride[0] == 0)
    sstride[0] = size;
  if (mstride[0] == 0)
    mstride[0] = 1;

  rstride0 = ret->dim[0].stride * size;
  if (rstride0 == 0)
    rstride0 = size;
  sstride0 = sstride[0];
  mstride0 = mstride[0];
  rptr = ret->data;
  sptr = array->data;
  mptr = mask->data;

  /* Use the same loop for both logical types. */
  if (GFC_DESCRIPTOR_SIZE (mask) != 4)
    {
      if (GFC_DESCRIPTOR_SIZE (mask) != 8)
        runtime_error ("Funny sized logical array");
      for (n = 0; n < dim; n++)
        mstride[n] <<= 1;
      mstride0 <<= 1;
      mptr = GFOR_POINTER_L8_TO_L4 (mptr);
    }

  while (sptr)
    {
      /* Test this element.  */
      if (*mptr)
        {
          /* Add it.  */
          memcpy (rptr, sptr, size);
          rptr += rstride0;
        }
      /* Advance to the next element.  */
      sptr += sstride0;
      mptr += mstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so proabably not worth it.  */
          sptr -= sstride[n] * extent[n];
          mptr -= mstride[n] * extent[n];
          n++;
          if (n >= dim)
            {
              /* Break out of the loop.  */
              sptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              sptr += sstride[n];
              mptr += mstride[n];
            }
        }
    }

  /* Add any remaining elements from VECTOR.  */
  if (vector)
    {
      n = vector->dim[0].ubound + 1 - vector->dim[0].lbound;
      nelem = ((rptr - ret->data) / rstride0);
      if (n > nelem)
        {
          sstride0 = vector->dim[0].stride * size;
          if (sstride0 == 0)
            sstride0 = size;

          sptr = vector->data + sstride0 * nelem;
          n -= nelem;
          while (n--)
            {
              memcpy (rptr, sptr, size);
              rptr += rstride0;
              sptr += sstride0;
            }
        }
    }
}

