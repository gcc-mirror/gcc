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

typedef GFC_ARRAY_DESCRIPTOR(GFC_MAX_DIMENSIONS, char) char_array;

/* Both parameters will already have been converted to the result type.  */
rtype_name
`__dot_product_'rtype_code (rtype * a, rtype * b)
{
  rtype_name *pa;
  rtype_name *pb;
  rtype_name res;
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
  res = 0;
  pa = a->data;
  pb = b->data;
sinclude(`dotprod_asm_'rtype_code`.m4')dnl

  while (count--)
    {
      res += *pa * *pb;
      pa += astride;
      pb += bstride;
    }

  return res;
}

