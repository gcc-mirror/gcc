/* Implementation of the DOT_PRODUCT intrinsic
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Feng Wang <fengwang@nudt.edu.cn>

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

#if defined (HAVE_GFC_COMPLEX_10)

typedef GFC_ARRAY_DESCRIPTOR(GFC_MAX_DIMENSIONS, char) char_array;

extern GFC_COMPLEX_10 dot_product_c10 (gfc_array_c10 * a, gfc_array_c10 * b);
export_proto(dot_product_c10);

/* Both parameters will already have been converted to the result type.  */
GFC_COMPLEX_10
dot_product_c10 (gfc_array_c10 * a, gfc_array_c10 * b)
{
  GFC_COMPLEX_10 *pa;
  GFC_COMPLEX_10 *pb;
  GFC_COMPLEX_10 res;
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

  while (count--)
    {
      res += __builtin_conjl (*pa) * *pb;
      pa += astride;
      pb += bstride;
    }

  return res;
}

#endif
