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
#include <string.h>
#include <assert.h>
#include "libgfortran.h"

typedef GFC_ARRAY_DESCRIPTOR(1, index_type) shape_type;
typedef GFC_ARRAY_DESCRIPTOR(GFC_MAX_DIMENSIONS, char) parray;


/* The shape parameter is ignored. We can currently deduce the shape from the
   return array.  */

void
__reshape (parray * ret, parray * source, shape_type * shape,
           parray * pad, shape_type * order)
{
  /* r.* indicates the return array.  */
  index_type rcount[GFC_MAX_DIMENSIONS - 1];
  index_type rextent[GFC_MAX_DIMENSIONS - 1];
  index_type rstride[GFC_MAX_DIMENSIONS - 1];
  index_type rstride0;
  index_type rdim;
  index_type rsize;
  char *rptr;
  /* s.* indicates the source array.  */
  index_type scount[GFC_MAX_DIMENSIONS - 1];
  index_type sextent[GFC_MAX_DIMENSIONS - 1];
  index_type sstride[GFC_MAX_DIMENSIONS - 1];
  index_type sstride0;
  index_type sdim;
  index_type ssize;
  const char *sptr;
  /* p.* indicates the pad array.  */
  index_type pcount[GFC_MAX_DIMENSIONS - 1];
  index_type pextent[GFC_MAX_DIMENSIONS - 1];
  index_type pstride[GFC_MAX_DIMENSIONS - 1];
  index_type pdim;
  index_type psize;
  const char *pptr;

  const char *src;
  int n;
  int dim;
  int size;

  size = GFC_DESCRIPTOR_SIZE (ret);
  if (ret->dim[0].stride == 0)
    ret->dim[0].stride = 1;
  if (source->dim[0].stride == 0)
    source->dim[0].stride = 1;
  if (shape->dim[0].stride == 0)
    shape->dim[0].stride = 1;
  if (pad && pad->dim[0].stride == 0)
    pad->dim[0].stride = 1;
  if (order && order->dim[0].stride == 0)
    order->dim[0].stride = 1;

  rdim = GFC_DESCRIPTOR_RANK (ret);
  rsize = 1;
  for (n = 0; n < rdim; n++)
    {
      if (order)
        dim = order->data[n * order->dim[0].stride] - 1;
      else
        dim = n;

      rcount[n] = 0;
      rstride[n] = ret->dim[dim].stride;
      rextent[n] = ret->dim[dim].ubound + 1 - ret->dim[dim].lbound;

      if (rextent[n] != shape->data[dim * shape->dim[0].stride])
        runtime_error ("shape and target do not conform");

      if (rsize == rstride[n])
        rsize *= rextent[n];
      else
        rsize = 0;
      if (rextent[dim] <= 0)
        return;
    }

  sdim = GFC_DESCRIPTOR_RANK (source);
  ssize = 1;
  for (n = 0; n < sdim; n++)
    {
      scount[n] = 0;
      sstride[n] = source->dim[n].stride;
      sextent[n] = source->dim[n].ubound + 1 - source->dim[n].lbound;
      if (sextent[n] <= 0)
        abort ();

      if (rsize == sstride[n])
        ssize *= sextent[n];
      else
        ssize = 0;
    }

  if (pad)
    {
      if (pad->dim[0].stride == 0)
        pad->dim[0].stride = 1;
      pdim = GFC_DESCRIPTOR_RANK (pad);
      psize = 1;
      for (n = 0; n < pdim; n++)
        {
          pcount[n] = 0;
          pstride[n] = pad->dim[n].stride;
          pextent[n] = pad->dim[n].ubound + 1 - pad->dim[n].lbound;
          if (pextent[n] <= 0)
            abort ();
          if (psize == pstride[n])
            psize *= pextent[n];
          else
            rsize = 0;
        }
      pptr = pad->data;
    }
  else
    {
      pdim = 0;
      psize = 1;
      pptr = NULL;
    }

  if (rsize != 0 && ssize != 0 && psize != 0)
    {
      rsize *= size;
      ssize *= size;
      psize *= size;
      reshape_packed (ret->data, rsize, source->data, ssize,
		      pad ? pad->data : NULL, psize);
      return;
    }
  rptr = ret->data;
  src = sptr = source->data;
  rstride0 = rstride[0] * size;
  sstride0 = sstride[0] * size;

  while (rptr)
    {
      /* Select between the source and pad arrays.  */
      memcpy(rptr, src, size);
      /* Advance to the next element.  */
      rptr += rstride0;
      src += sstride0;
      rcount[0]++;
      scount[0]++;
      /* Advance to the next destination element.  */
      n = 0;
      while (rcount[n] == rextent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          rcount[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so proabably not worth it.  */
          rptr -= rstride[n] * rextent[n] * size;
          n++;
          if (n == rdim)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              rcount[n]++;
              rptr += rstride[n] * size;
            }
        }
      /* Advance to the next source element.  */
      n = 0;
      while (scount[n] == sextent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          scount[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so proabably not worth it.  */
          src -= sstride[n] * sextent[n] * size;
          n++;
          if (n == sdim)
            {
              if (sptr && pad)
                {
                  /* Switch to the pad array.  */
                  sptr = NULL;
                  sdim = pdim;
                  for (dim = 0; dim < pdim; dim++)
                    {
                      scount[dim] = pcount[dim];
                      sextent[dim] = pextent[dim];
                      sstride[dim] = pstride[dim];
                      sstride0 = sstride[0] * size;
                    }
                }
              /* We now start again from the beginning of the pad array.  */
              src = pptr;
              break;
            }
          else
            {
              scount[n]++;
              sptr += sstride[n] * size;
            }
        }
    }
}

