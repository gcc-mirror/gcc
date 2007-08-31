/* Implementation of the TRANSPOSE intrinsic
   Copyright 2003, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter

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

#include "libgfortran.h"
#include <assert.h>


#if defined (HAVE_GFC_COMPLEX_4)

extern void transpose_c4 (gfc_array_c4 * const restrict ret, 
	gfc_array_c4 * const restrict source);
export_proto(transpose_c4);

void
transpose_c4 (gfc_array_c4 * const restrict ret, 
	gfc_array_c4 * const restrict source)
{
  /* r.* indicates the return array.  */
  index_type rxstride, rystride;
  GFC_COMPLEX_4 *rptr;
  /* s.* indicates the source array.  */
  index_type sxstride, systride;
  const GFC_COMPLEX_4 *sptr;

  index_type xcount, ycount;
  index_type x, y;

  assert (GFC_DESCRIPTOR_RANK (source) == 2);

  if (ret->data == NULL)
    {
      assert (GFC_DESCRIPTOR_RANK (ret) == 2);
      assert (ret->dtype == source->dtype);

      ret->dim[0].lbound = 0;
      ret->dim[0].ubound = source->dim[1].ubound - source->dim[1].lbound;
      ret->dim[0].stride = 1;

      ret->dim[1].lbound = 0;
      ret->dim[1].ubound = source->dim[0].ubound - source->dim[0].lbound;
      ret->dim[1].stride = ret->dim[0].ubound+1;

      ret->data = internal_malloc_size (sizeof (GFC_COMPLEX_4) * size0 ((array_t *) ret));
      ret->offset = 0;
    }

  sxstride = source->dim[0].stride;
  systride = source->dim[1].stride;
  xcount = source->dim[0].ubound + 1 - source->dim[0].lbound;
  ycount = source->dim[1].ubound + 1 - source->dim[1].lbound;

  rxstride = ret->dim[0].stride;
  rystride = ret->dim[1].stride;

  rptr = ret->data;
  sptr = source->data;

  for (y=0; y < ycount; y++)
    {
      for (x=0; x < xcount; x++)
        {
          *rptr = *sptr;

          sptr += sxstride;
          rptr += rystride;
        }
        sptr += systride - (sxstride * xcount);
        rptr += rxstride - (rystride * xcount);
    }
}

#endif
