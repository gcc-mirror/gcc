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
__spread (const gfc_array_char * ret, const gfc_array_char * source,
    const index_type * along, const index_type * pncopies)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS - 1];
  index_type rstride0;
  index_type rdelta;
  char *rptr;
  char *dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS - 1];
  index_type sstride0;
  const char *sptr;

  index_type count[GFC_MAX_DIMENSIONS - 1];
  index_type extent[GFC_MAX_DIMENSIONS - 1];
  index_type n;
  index_type dim;
  index_type size;
  index_type ncopies;

  size = GFC_DESCRIPTOR_SIZE (source);
  dim = 0;
  for (n = 0; n < GFC_DESCRIPTOR_RANK (ret); n++)
    {
      if (n == *along - 1)
        {
          rdelta = ret->dim[n].stride * size;
        }
      else
        {
          count[dim] = 0;
          extent[dim] = source->dim[dim].ubound + 1 - source->dim[dim].lbound;
          sstride[dim] = source->dim[dim].stride * size;
          rstride[dim] = ret->dim[n].stride * size;
          dim++;
        }
    }
  dim = GFC_DESCRIPTOR_RANK (source);
  if (sstride[0] == 0)
    sstride[0] = size;
  if (rstride[0] == 0)
    rstride[0] = size;

  sstride0 = sstride[0];
  rstride0 = rstride[0];
  rptr = ret->data;
  sptr = source->data;
  ncopies = *pncopies;

  while (sptr)
    {
      /* Spread this element.  */
      dest = rptr;
      for (n = 0; n < ncopies; n++)
        {
          memcpy (dest, sptr, size);
          dest += rdelta;
        }
      /* Advance to the next element.  */
      sptr += sstride0;
      rptr += rstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          sptr -= sstride[n] * extent[n];
          rptr -= rstride[n] * extent[n];
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
              rptr += rstride[n];
            }
        }
    }
}

