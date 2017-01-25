/* Helper function for repacking arrays.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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


#if defined (HAVE_GFC_REAL_16)

void
internal_unpack_r16 (gfc_array_r16 * d, const GFC_REAL_16 * src)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  index_type dsize;
  GFC_REAL_16 * restrict dest;
  int n;

  dest = d->base_addr;
  if (src == dest || !src)
    return;

  dim = GFC_DESCRIPTOR_RANK (d);
  dsize = 1;
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE(d,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(d,n);
      if (extent[n] <= 0)
	return;

      if (dsize == stride[n])
	dsize *= extent[n];
      else
	dsize = 0;
    }

  if (dsize != 0)
    {
      memcpy (dest, src, dsize * sizeof (GFC_REAL_16));
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

