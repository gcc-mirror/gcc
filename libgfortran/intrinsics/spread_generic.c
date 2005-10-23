/* Generic implementation of the SPREAD intrinsic
   Copyright 2002, 2005 Free Software Foundation, Inc.
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

#include "config.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "libgfortran.h"

static void
spread_internal (gfc_array_char *ret, const gfc_array_char *source,
		 const index_type *along, const index_type *pncopies,
		 index_type size)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type rdelta = 0;
  index_type rrank;
  index_type rs;
  char *rptr;
  char *dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type srank;
  const char *sptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type n;
  index_type dim;
  index_type ncopies;

  srank = GFC_DESCRIPTOR_RANK(source);

  rrank = srank + 1;
  if (rrank > GFC_MAX_DIMENSIONS)
    runtime_error ("return rank too large in spread()");

  if (*along > rrank)
      runtime_error ("dim outside of rank in spread()");

  ncopies = *pncopies;

  if (ret->data == NULL)
    {
      /* The front end has signalled that we need to populate the
	 return array descriptor.  */
      ret->dtype = (source->dtype & ~GFC_DTYPE_RANK_MASK) | rrank;
      dim = 0;
      rs = 1;
      for (n = 0; n < rrank; n++)
	{
	  ret->dim[n].stride = rs;
	  ret->dim[n].lbound = 0;
	  if (n == *along - 1)
	    {
	      ret->dim[n].ubound = ncopies - 1;
	      rdelta = rs * size;
	      rs *= ncopies;
	    }
	  else
	    {
	      count[dim] = 0;
	      extent[dim] = source->dim[dim].ubound + 1
		- source->dim[dim].lbound;
	      sstride[dim] = source->dim[dim].stride * size;
	      rstride[dim] = rs * size;

	      ret->dim[n].ubound = extent[dim]-1;
	      rs *= extent[dim];
	      dim++;
	    }
	}
      ret->offset = 0;
      ret->data = internal_malloc_size (rs * size);
    }
  else
    {
      dim = 0;
      if (GFC_DESCRIPTOR_RANK(ret) != rrank)
	runtime_error ("rank mismatch in spread()");

      if (ret->dim[0].stride == 0)
	ret->dim[0].stride = 1;

      for (n = 0; n < rrank; n++)
	{
	  if (n == *along - 1)
	    {
	      rdelta = ret->dim[n].stride * size;
	    }
	  else
	    {
	      count[dim] = 0;
	      extent[dim] = source->dim[dim].ubound + 1
		- source->dim[dim].lbound;
	      sstride[dim] = source->dim[dim].stride * size;
	      rstride[dim] = ret->dim[n].stride * size;
	      dim++;
	    }
	}
      if (sstride[0] == 0)
	sstride[0] = size;
    }
  sstride0 = sstride[0];
  rstride0 = rstride[0];
  rptr = ret->data;
  sptr = source->data;

  while (sptr)
    {
      /* Spread this element.  */
      dest = rptr;
      for (n = 0; n < ncopies; n++)
        {
          memcpy (dest, sptr, size);
          dest += rdelta;
        }
      /* Advance to the next element.  */
      sptr += sstride0;
      rptr += rstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so probably not worth it.  */
          sptr -= sstride[n] * extent[n];
          rptr -= rstride[n] * extent[n];
          n++;
          if (n >= srank)
            {
              /* Break out of the loop.  */
              sptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              sptr += sstride[n];
              rptr += rstride[n];
            }
        }
    }
}

/* This version of spread_internal treats the special case of a scalar
   source.  This is much simpler than the more general case above.  */

static void
spread_internal_scalar (gfc_array_char *ret, const char *source,
			const index_type *along, const index_type *pncopies,
			index_type size)
{
  int n;
  int ncopies = *pncopies;
  char * dest;

  if (GFC_DESCRIPTOR_RANK (ret) != 1)
    runtime_error ("incorrect destination rank in spread()");

  if (*along > 1)
    runtime_error ("dim outside of rank in spread()");

  if (ret->data == NULL)
    {
      ret->data = internal_malloc_size (ncopies * size);
      ret->offset = 0;
      ret->dim[0].stride = 1;
      ret->dim[0].lbound = 0;
      ret->dim[0].ubound = ncopies - 1;
    }
  else
    {
      if (ret->dim[0].stride == 0)
	ret->dim[0].stride = 1;

      if (ncopies - 1 > (ret->dim[0].ubound - ret->dim[0].lbound)
			   / ret->dim[0].stride)
	runtime_error ("dim too large in spread()");
    }

  for (n = 0; n < ncopies; n++)
    {
      dest = (char*)(ret->data + n*size*ret->dim[0].stride);
      memcpy (dest , source, size);
    }
}

extern void spread (gfc_array_char *, const gfc_array_char *,
		    const index_type *, const index_type *);
export_proto(spread);

void
spread (gfc_array_char *ret, const gfc_array_char *source,
	const index_type *along, const index_type *pncopies)
{
  spread_internal (ret, source, along, pncopies, GFC_DESCRIPTOR_SIZE (source));
}

extern void spread_char (gfc_array_char *, GFC_INTEGER_4,
			 const gfc_array_char *, const index_type *,
			 const index_type *, GFC_INTEGER_4);
export_proto(spread_char);

void
spread_char (gfc_array_char *ret,
	     GFC_INTEGER_4 ret_length __attribute__((unused)),
	     const gfc_array_char *source, const index_type *along,
	     const index_type *pncopies, GFC_INTEGER_4 source_length)
{
  spread_internal (ret, source, along, pncopies, source_length);
}

/* The following are the prototypes for the versions of spread with a
   scalar source.  */

extern void spread_scalar (gfc_array_char *, const char *,
			   const index_type *, const index_type *);
export_proto(spread_scalar);

void
spread_scalar (gfc_array_char *ret, const char *source,
	       const index_type *along, const index_type *pncopies)
{
  if (!ret->dtype)
    runtime_error ("return array missing descriptor in spread()");
  spread_internal_scalar (ret, source, along, pncopies, GFC_DESCRIPTOR_SIZE (ret));
}


extern void spread_char_scalar (gfc_array_char *, GFC_INTEGER_4,
				const char *, const index_type *,
				const index_type *, GFC_INTEGER_4);
export_proto(spread_char_scalar);

void
spread_char_scalar (gfc_array_char *ret,
		    GFC_INTEGER_4 ret_length __attribute__((unused)),
		    const char *source, const index_type *along,
		    const index_type *pncopies, GFC_INTEGER_4 source_length)
{
  if (!ret->dtype)
    runtime_error ("return array missing descriptor in spread()");
  spread_internal_scalar (ret, source, along, pncopies, source_length);
}

