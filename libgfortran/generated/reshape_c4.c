/* Implementation of the RESHAPE
   Copyright 2002 Free Software Foundation, Inc.
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

#if defined (HAVE_GFC_COMPLEX_4)

typedef GFC_ARRAY_DESCRIPTOR(1, index_type) shape_type;

/* The shape parameter is ignored. We can currently deduce the shape from the
   return array.  */

extern void reshape_c4 (gfc_array_c4 *, gfc_array_c4 *, shape_type *,
				    gfc_array_c4 *, shape_type *);
export_proto(reshape_c4);

void
reshape_c4 (gfc_array_c4 * ret, gfc_array_c4 * source, shape_type * shape,
                      gfc_array_c4 * pad, shape_type * order)
{
  /* r.* indicates the return array.  */
  index_type rcount[GFC_MAX_DIMENSIONS];
  index_type rextent[GFC_MAX_DIMENSIONS];
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rdim;
  index_type rsize;
  index_type rs;
  index_type rex;
  GFC_COMPLEX_4 *rptr;
  /* s.* indicates the source array.  */
  index_type scount[GFC_MAX_DIMENSIONS];
  index_type sextent[GFC_MAX_DIMENSIONS];
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type sdim;
  index_type ssize;
  const GFC_COMPLEX_4 *sptr;
  /* p.* indicates the pad array.  */
  index_type pcount[GFC_MAX_DIMENSIONS];
  index_type pextent[GFC_MAX_DIMENSIONS];
  index_type pstride[GFC_MAX_DIMENSIONS];
  index_type pdim;
  index_type psize;
  const GFC_COMPLEX_4 *pptr;

  const GFC_COMPLEX_4 *src;
  int n;
  int dim;

  if (source->dim[0].stride == 0)
    source->dim[0].stride = 1;
  if (shape->dim[0].stride == 0)
    shape->dim[0].stride = 1;
  if (pad && pad->dim[0].stride == 0)
    pad->dim[0].stride = 1;
  if (order && order->dim[0].stride == 0)
    order->dim[0].stride = 1;

  if (ret->data == NULL)
    {
      rdim = shape->dim[0].ubound - shape->dim[0].lbound + 1;
      rs = 1;
      for (n=0; n < rdim; n++)
	{
	  ret->dim[n].lbound = 0;
	  rex = shape->data[n * shape->dim[0].stride];
	  ret->dim[n].ubound =  rex - 1;
	  ret->dim[n].stride = rs;
	  rs *= rex;
	}
      ret->offset = 0;
      ret->data = internal_malloc_size ( rs * sizeof (GFC_COMPLEX_4));
      ret->dtype = (source->dtype & ~GFC_DTYPE_RANK_MASK) | rdim;
    }
  else
    {
      rdim = GFC_DESCRIPTOR_RANK (ret);
      if (ret->dim[0].stride == 0)
	ret->dim[0].stride = 1;
    }

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
      if (rextent[n] <= 0)
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

      if (ssize == sstride[n])
        ssize *= sextent[n];
      else
        ssize = 0;
    }

  if (pad)
    {
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
            psize = 0;
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
      rsize *= sizeof (GFC_COMPLEX_4);
      ssize *= sizeof (GFC_COMPLEX_4);
      psize *= sizeof (GFC_COMPLEX_4);
      reshape_packed ((char *)ret->data, rsize, (char *)source->data,
		      ssize, pad ? (char *)pad->data : NULL, psize);
      return;
    }
  rptr = ret->data;
  src = sptr = source->data;
  rstride0 = rstride[0];
  sstride0 = sstride[0];

  while (rptr)
    {
      /* Select between the source and pad arrays.  */
      *rptr = *src;
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
          rptr -= rstride[n] * rextent[n];
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
              rptr += rstride[n];
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
          src -= sstride[n] * sextent[n];
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
                      sstride0 = sstride[0];
                    }
                }
              /* We now start again from the beginning of the pad array.  */
              src = pptr;
              break;
            }
          else
            {
              scount[n]++;
              src += sstride[n];
            }
        }
    }
}

#endif
