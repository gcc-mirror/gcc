/* Specific implementation of the UNPACK intrinsic
   Copyright 2008, 2009 Free Software Foundation, Inc.
   Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>, based on
   unpack_generic.c by Paul Brook <paul@nowt.org>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Ligbfortran is distributed in the hope that it will be useful,
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
#include <assert.h>
#include <string.h>


#if defined (HAVE_GFC_COMPLEX_16)

void
unpack0_c16 (gfc_array_c16 *ret, const gfc_array_c16 *vector,
		 const gfc_array_l1 *mask, const GFC_COMPLEX_16 *fptr)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rs;
  GFC_COMPLEX_16 * restrict rptr;
  /* v.* indicates the vector array.  */
  index_type vstride0;
  GFC_COMPLEX_16 *vptr;
  /* Value for field, this is constant.  */
  const GFC_COMPLEX_16 fval = *fptr;
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
      /*  Do not convert a NULL pointer as we use test for NULL below.  */
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
	  rstride[n] = ret->dim[n].stride;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	  rs *= extent[n];
	}
      ret->offset = 0;
      ret->data = internal_malloc_size (rs * sizeof (GFC_COMPLEX_16));
    }
  else
    {
      dim = GFC_DESCRIPTOR_RANK (ret);
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  extent[n] = ret->dim[n].ubound + 1 - ret->dim[n].lbound;
	  empty = empty || extent[n] <= 0;
	  rstride[n] = ret->dim[n].stride;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	}
      if (rstride[0] == 0)
	rstride[0] = 1;
    }

  if (empty)
    return;

  if (mstride[0] == 0)
    mstride[0] = 1;

  vstride0 = vector->dim[0].stride;
  if (vstride0 == 0)
    vstride0 = 1;
  rstride0 = rstride[0];
  mstride0 = mstride[0];
  rptr = ret->data;
  vptr = vector->data;

  while (rptr)
    {
      if (*mptr)
        {
	  /* From vector.  */
	  *rptr = *vptr;
	  vptr += vstride0;
        }
      else
        {
	  /* From field.  */
	  *rptr = fval;
        }
      /* Advance to the next element.  */
      rptr += rstride0;
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
              mptr += mstride[n];
            }
        }
    }
}

void
unpack1_c16 (gfc_array_c16 *ret, const gfc_array_c16 *vector,
		 const gfc_array_l1 *mask, const gfc_array_c16 *field)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rs;
  GFC_COMPLEX_16 * restrict rptr;
  /* v.* indicates the vector array.  */
  index_type vstride0;
  GFC_COMPLEX_16 *vptr;
  /* f.* indicates the field array.  */
  index_type fstride[GFC_MAX_DIMENSIONS];
  index_type fstride0;
  const GFC_COMPLEX_16 *fptr;
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
      /*  Do not convert a NULL pointer as we use test for NULL below.  */
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
	  rstride[n] = ret->dim[n].stride;
	  fstride[n] = field->dim[n].stride;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	  rs *= extent[n];
	}
      ret->offset = 0;
      ret->data = internal_malloc_size (rs * sizeof (GFC_COMPLEX_16));
    }
  else
    {
      dim = GFC_DESCRIPTOR_RANK (ret);
      for (n = 0; n < dim; n++)
	{
	  count[n] = 0;
	  extent[n] = ret->dim[n].ubound + 1 - ret->dim[n].lbound;
	  empty = empty || extent[n] <= 0;
	  rstride[n] = ret->dim[n].stride;
	  fstride[n] = field->dim[n].stride;
	  mstride[n] = mask->dim[n].stride * mask_kind;
	}
      if (rstride[0] == 0)
	rstride[0] = 1;
    }

  if (empty)
    return;

  if (fstride[0] == 0)
    fstride[0] = 1;
  if (mstride[0] == 0)
    mstride[0] = 1;

  vstride0 = vector->dim[0].stride;
  if (vstride0 == 0)
    vstride0 = 1;
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
	  *rptr = *vptr;
          vptr += vstride0;
        }
      else
        {
          /* From field.  */
	  *rptr = *fptr;
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

#endif

