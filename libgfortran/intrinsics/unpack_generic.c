/* Generic implementation of the UNPACK intrinsic
   Copyright 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
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

Ligbfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

static void
unpack_internal (gfc_array_char *ret, const gfc_array_char *vector,
		 const gfc_array_l1 *mask, const gfc_array_char *field,
		 index_type size, index_type fsize)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rs;
  char *rptr;
  /* v.* indicates the vector array.  */
  index_type vstride0;
  char *vptr;
  /* f.* indicates the field array.  */
  index_type fstride[GFC_MAX_DIMENSIONS];
  index_type fstride0;
  const char *fptr;
  /* m.* indicates the mask array.  */
  index_type mstride[GFC_MAX_DIMENSIONS];
  index_type mstride0;
  const GFC_LOGICAL_1 *mptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type n;
  index_type dim;

  int empty;
  int mask_kind;

  empty = 0;

  mptr = mask->data;

  /* Use the same loop for all logical types, by using GFC_LOGICAL_1
     and using shifting to address size and endian issues.  */

  mask_kind = GFC_DESCRIPTOR_SIZE (mask);

  if (mask_kind == 1 || mask_kind == 2 || mask_kind == 4 || mask_kind == 8
#ifdef HAVE_GFC_LOGICAL_16
      || mask_kind == 16
#endif
      )
    {
      /*  Don't convert a NULL pointer as we use test for NULL below.  */
      if (mptr)
	mptr = GFOR_POINTER_TO_L1 (mptr, mask_kind);
    }
  else
    runtime_error ("Funny sized logical array");

  if (ret->data == NULL)
    {
      /* The front end has signalled that we need to populate the
	 return array descriptor.  */
      dim = GFC_DESCRIPTOR_RANK (mask);
      rs = 1;
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  ret->dim[n].stride = rs;
	  ret->dim[n].lbound = 0;
	  ret->dim[n].ubound = mask->dim[n].ubound - mask->dim[n].lbound;
	  extent[n] = ret->dim[n].ubound + 1;
	  empty = empty || extent[n] <= 0;
	  rstride[n] = ret->dim[n].stride * size;
	  fstride[n] = field->dim[n].stride * fsize;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	  rs *= extent[n];
	}
      ret->offset = 0;
      ret->data = internal_malloc_size (rs * size);
    }
  else
    {
      dim = GFC_DESCRIPTOR_RANK (ret);
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  extent[n] = ret->dim[n].ubound + 1 - ret->dim[n].lbound;
	  empty = empty || extent[n] <= 0;
	  rstride[n] = ret->dim[n].stride * size;
	  fstride[n] = field->dim[n].stride * fsize;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	}
      if (rstride[0] == 0)
	rstride[0] = size;
    }

  if (empty)
    return;

  if (fstride[0] == 0)
    fstride[0] = fsize;
  if (mstride[0] == 0)
    mstride[0] = 1;

  vstride0 = vector->dim[0].stride * size;
  if (vstride0 == 0)
    vstride0 = size;
  rstride0 = rstride[0];
  fstride0 = fstride[0];
  mstride0 = mstride[0];
  rptr = ret->data;
  fptr = field->data;
  vptr = vector->data;

  while (rptr)
    {
      if (*mptr)
        {
          /* From vector.  */
          memcpy (rptr, vptr, size);
          vptr += vstride0;
        }
      else
        {
          /* From field.  */
          memcpy (rptr, fptr, size);
        }
      /* Advance to the next element.  */
      rptr += rstride0;
      fptr += fstride0;
      mptr += mstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          rptr -= rstride[n] * extent[n];
          fptr -= fstride[n] * extent[n];
          mptr -= mstride[n] * extent[n];
          n++;
          if (n >= dim)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              rptr += rstride[n];
              fptr += fstride[n];
              mptr += mstride[n];
            }
        }
    }
}

extern void unpack1 (gfc_array_char *, const gfc_array_char *,
		     const gfc_array_l1 *, const gfc_array_char *);
export_proto(unpack1);

void
unpack1 (gfc_array_char *ret, const gfc_array_char *vector,
	 const gfc_array_l1 *mask, const gfc_array_char *field)
{
  unpack_internal (ret, vector, mask, field,
		   GFC_DESCRIPTOR_SIZE (vector),
		   GFC_DESCRIPTOR_SIZE (field));
}

extern void unpack1_char (gfc_array_char *, GFC_INTEGER_4,
			  const gfc_array_char *, const gfc_array_l1 *,
			  const gfc_array_char *, GFC_INTEGER_4,
			  GFC_INTEGER_4);
export_proto(unpack1_char);

void
unpack1_char (gfc_array_char *ret,
	      GFC_INTEGER_4 ret_length __attribute__((unused)),
	      const gfc_array_char *vector, const gfc_array_l1 *mask,
	      const gfc_array_char *field, GFC_INTEGER_4 vector_length,
	      GFC_INTEGER_4 field_length)
{
  unpack_internal (ret, vector, mask, field, vector_length, field_length);
}

extern void unpack0 (gfc_array_char *, const gfc_array_char *,
		     const gfc_array_l1 *, char *);
export_proto(unpack0);

void
unpack0 (gfc_array_char *ret, const gfc_array_char *vector,
	 const gfc_array_l1 *mask, char *field)
{
  gfc_array_char tmp;

  memset (&tmp, 0, sizeof (tmp));
  tmp.dtype = 0;
  tmp.data = field;
  unpack_internal (ret, vector, mask, &tmp, GFC_DESCRIPTOR_SIZE (vector), 0);
}

extern void unpack0_char (gfc_array_char *, GFC_INTEGER_4,
			  const gfc_array_char *, const gfc_array_l1 *,
			  char *, GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(unpack0_char);

void
unpack0_char (gfc_array_char *ret,
	      GFC_INTEGER_4 ret_length __attribute__((unused)),
	      const gfc_array_char *vector, const gfc_array_l1 *mask,
	      char *field, GFC_INTEGER_4 vector_length,
	      GFC_INTEGER_4 field_length __attribute__((unused)))
{
  gfc_array_char tmp;

  memset (&tmp, 0, sizeof (tmp));
  tmp.dtype = 0;
  tmp.data = field;
  unpack_internal (ret, vector, mask, &tmp, vector_length, 0);
}
