`/* Implementation of the DOT_PRODUCT intrinsic
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
#include "libgfortran.h"'
include(iparm.m4)dnl

rtype_name
`__dot_product_'rtype_code (gfc_array_l4 * a, gfc_array_l4 * b)
{
  GFC_LOGICAL_4 *pa;
  GFC_LOGICAL_4 *pb;
  index_type count;
  index_type astride;
  index_type bstride;

  assert (GFC_DESCRIPTOR_RANK (a) == 1
          && GFC_DESCRIPTOR_RANK (b) == 1);

  if (a->dim[0].stride == 0)
    a->dim[0].stride = 1;
  if (b->dim[0].stride == 0)
    b->dim[0].stride = 1;

  astride = a->dim[0].stride;
  bstride = b->dim[0].stride;
  count = a->dim[0].ubound + 1 - a->dim[0].lbound;

  pa = a->data;
  if (GFC_DESCRIPTOR_SIZE (a) != 4)
    {
      assert (GFC_DESCRIPTOR_SIZE (a) == 8);
      pa = GFOR_POINTER_L8_TO_L4 (pa);
      astride <<= 1;
    }
  pb = b->data;
  if (GFC_DESCRIPTOR_SIZE (b) != 4)
    {
      assert (GFC_DESCRIPTOR_SIZE (b) == 8);
      pb = GFOR_POINTER_L8_TO_L4 (pb);
      bstride <<= 1;
    }

  while (count--)
    {
      if (*pa && *pb)
        return 1;

      pa += astride;
      pb += bstride;
    }

  return 0;
}

