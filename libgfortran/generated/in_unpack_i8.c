/* Helper function for repacking arrays.
   Copyright 2003, 2006 Free Software Foundation, Inc.
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
#include <string.h>
#include "libgfortran.h"

#if defined (HAVE_GFC_INTEGER_8)

void
internal_unpack_8 (gfc_array_i8 * d, const GFC_INTEGER_8 * src)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  index_type dsize;
  GFC_INTEGER_8 *dest;
  int n;

  dest = d->data;
  if (src == dest || !src)
    return;

  dim = GFC_DESCRIPTOR_RANK (d);
  dsize = 1;
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = d->dim[n].stride;
      extent[n] = d->dim[n].ubound + 1 - d->dim[n].lbound;
      if (extent[n] <= 0)
        abort ();

      if (dsize == stride[n])
        dsize *= extent[n];
      else
        dsize = 0;
    }

  if (dsize != 0)
    {
      memcpy (dest, src, dsize * sizeof (GFC_INTEGER_8));
      return;
    }

  stride0 = stride[0];

  while (dest)
    {
      /* Copy the data.  */
      *dest = *(src++);
      /* Advance to the next element.  */
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          dest -= stride[n] * extent[n];
          n++;
          if (n == dim)
            {
              dest = NULL;
              break;
            }
          else
            {
              count[n]++;
              dest += stride[n];
            }
        }
    }
}

#endif
