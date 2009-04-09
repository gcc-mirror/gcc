`/* Helper function for repacking arrays.
   Copyright 2003, 2006, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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
#include <stdlib.h>
#include <assert.h>'

include(iparm.m4)dnl

`#if defined (HAVE_'rtype_name`)

/* Allocates a block of memory with internal_malloc if the array needs
   repacking.  */
'
dnl The kind (ie size) is used to name the function for logicals, integers
dnl and reals.  For complex, it's c4 or c8.
rtype_name` *
internal_pack_'rtype_ccode` ('rtype` * source)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  index_type ssize;
  const 'rtype_name` *src;
  'rtype_name` * restrict dest;
  'rtype_name` *destptr;
  int n;
  int packed;

  /* TODO: Investigate how we can figure out if this is a temporary
     since the stride=0 thing has been removed from the frontend.  */

  dim = GFC_DESCRIPTOR_RANK (source);
  ssize = 1;
  packed = 1;
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = source->dim[n].stride;
      extent[n] = source->dim[n].ubound + 1 - source->dim[n].lbound;
      if (extent[n] <= 0)
        {
          /* Do nothing.  */
          packed = 1;
          break;
        }

      if (ssize != stride[n])
        packed = 0;

      ssize *= extent[n];
    }

  if (packed)
    return source->data;

  /* Allocate storage for the destination.  */
  destptr = ('rtype_name` *)internal_malloc_size (ssize * sizeof ('rtype_name`));
  dest = destptr;
  src = source->data;
  stride0 = stride[0];


  while (src)
    {
      /* Copy the data.  */
      *(dest++) = *src;
      /* Advance to the next element.  */
      src += stride0;
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
          src -= stride[n] * extent[n];
          n++;
          if (n == dim)
            {
              src = NULL;
              break;
            }
          else
            {
              count[n]++;
              src += stride[n];
            }
        }
    }
  return destptr;
}

#endif
'
