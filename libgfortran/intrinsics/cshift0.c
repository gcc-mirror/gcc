/* Generic implementation of the CSHIFT intrinsic
   Copyright 2003, 2005 Free Software Foundation, Inc.
   Contributed by Feng Wang <wf_cs@yahoo.com>

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


/* "Templatized" helper function for the inner shift loop.  */

#define DEF_COPY_LOOP(NAME, TYPE)					\
static inline void							\
copy_loop_##NAME (void *xdest, const void *xsrc,			\
		  size_t roff, size_t soff,				\
		  index_type len, index_type shift)			\
{									\
  TYPE *dest = xdest;							\
  const TYPE *src;							\
  index_type i;								\
									\
  roff /= sizeof (TYPE);						\
  soff /= sizeof (TYPE);						\
									\
  src = xsrc;								\
  src += shift * soff;							\
  for (i = 0; i < len - shift; ++i)					\
    {									\
      *dest = *src;							\
      dest += roff;							\
      src += soff;							\
    }									\
									\
  src = xsrc;								\
  for (i = 0; i < shift; ++i)						\
    {									\
      *dest = *src;							\
      dest += roff;							\
      src += soff;							\
    }									\
}

DEF_COPY_LOOP(int, int)
DEF_COPY_LOOP(long, long)
DEF_COPY_LOOP(double, double)
DEF_COPY_LOOP(ldouble, long double)
DEF_COPY_LOOP(cfloat, _Complex float)
DEF_COPY_LOOP(cdouble, _Complex double)


static void
cshift0 (gfc_array_char * ret, const gfc_array_char * array,
	 ssize_t shift, int which)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type roffset;
  char *rptr;

  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type soffset;
  const char *sptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dim;
  index_type size;
  index_type len;
  index_type n;
  int whichloop;

  if (which < 1 || which > GFC_DESCRIPTOR_RANK (array))
    runtime_error ("Argument 'DIM' is out of range in call to 'CSHIFT'");

  which = which - 1;

  extent[0] = 1;
  count[0] = 0;
  size = GFC_DESCRIPTOR_SIZE (array);
  n = 0;

  /* The values assigned here must match the cases in the inner loop.  */
  whichloop = 0;
  switch (GFC_DESCRIPTOR_TYPE (array))
    {
    case GFC_DTYPE_LOGICAL:
    case GFC_DTYPE_INTEGER:
    case GFC_DTYPE_REAL:
      if (size == sizeof (int))
	whichloop = 1;
      else if (size == sizeof (long))
	whichloop = 2;
      else if (size == sizeof (double))
	whichloop = 3;
      else if (size == sizeof (long double))
	whichloop = 4;
      break;

    case GFC_DTYPE_COMPLEX:
      if (size == sizeof (_Complex float))
	whichloop = 5;
      else if (size == sizeof (_Complex double))
	whichloop = 6;
      break;

    default:
      break;
    }

  /* Initialized for avoiding compiler warnings.  */
  roffset = size;
  soffset = size;
  len = 0;

  if (ret->data == NULL)
    {
      int i;

      ret->data = internal_malloc_size (size * size0 ((array_t *)array));
      ret->offset = 0;
      ret->dtype = array->dtype;
      for (i = 0; i < GFC_DESCRIPTOR_RANK (array); i++)
        {
          ret->dim[i].lbound = 0;
          ret->dim[i].ubound = array->dim[i].ubound - array->dim[i].lbound;

          if (i == 0)
            ret->dim[i].stride = 1;
          else
            ret->dim[i].stride = (ret->dim[i-1].ubound + 1) * ret->dim[i-1].stride;
        }
    }

  for (dim = 0; dim < GFC_DESCRIPTOR_RANK (array); dim++)
    {
      if (dim == which)
        {
          roffset = ret->dim[dim].stride * size;
          if (roffset == 0)
            roffset = size;
          soffset = array->dim[dim].stride * size;
          if (soffset == 0)
            soffset = size;
          len = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
        }
      else
        {
          count[n] = 0;
          extent[n] = array->dim[dim].ubound + 1 - array->dim[dim].lbound;
          rstride[n] = ret->dim[dim].stride * size;
          sstride[n] = array->dim[dim].stride * size;
          n++;
        }
    }
  if (sstride[0] == 0)
    sstride[0] = size;
  if (rstride[0] == 0)
    rstride[0] = size;

  dim = GFC_DESCRIPTOR_RANK (array);
  rstride0 = rstride[0];
  sstride0 = sstride[0];
  rptr = ret->data;
  sptr = array->data;

  shift = shift % (ssize_t)len;
  if (shift < 0)
    shift += len;

  while (rptr)
    {
      /* Do the shift for this dimension.  */

      /* If elements are contiguous, perform the operation
	 in two block moves.  */
      if (soffset == size && roffset == size)
	{
	  size_t len1 = shift * size;
	  size_t len2 = (len - shift) * size;
	  memcpy (rptr, sptr + len1, len2);
	  memcpy (rptr + len2, sptr, len1);
	}
      else
	{
	  /* Otherwise, we'll have to perform the copy one element at
	     a time.  We can speed this up a tad for common cases of 
	     fundamental types.  */
	  switch (whichloop)
	    {
	    case 0:
	      {
		char *dest = rptr;
		const char *src = &sptr[shift * soffset];

		for (n = 0; n < len - shift; n++)
		  {
		    memcpy (dest, src, size);
		    dest += roffset;
		    src += soffset;
		  }
		for (src = sptr, n = 0; n < shift; n++)
		  {
		    memcpy (dest, src, size);
		    dest += roffset;
		    src += soffset;
		  }
	      }
	      break;

	    case 1:
	      copy_loop_int (rptr, sptr, roffset, soffset, len, shift);
	      break;

	    case 2:
	      copy_loop_long (rptr, sptr, roffset, soffset, len, shift);
	      break;

	    case 3:
	      copy_loop_double (rptr, sptr, roffset, soffset, len, shift);
	      break;

	    case 4:
	      copy_loop_ldouble (rptr, sptr, roffset, soffset, len, shift);
	      break;

	    case 5:
	      copy_loop_cfloat (rptr, sptr, roffset, soffset, len, shift);
	      break;
	      
	    case 6:
	      copy_loop_cdouble (rptr, sptr, roffset, soffset, len, shift);
	      break;

	    default:
	      abort ();
	    }
	}

      /* Advance to the next section.  */
      rptr += rstride0;
      sptr += sstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          /* We could precalculate these products, but this is a less
             frequently used path so proabably not worth it.  */
          rptr -= rstride[n] * extent[n];
          sptr -= sstride[n] * extent[n];
          n++;
          if (n >= dim - 1)
            {
              /* Break out of the loop.  */
              rptr = NULL;
              break;
            }
          else
            {
              count[n]++;
              rptr += rstride[n];
              sptr += sstride[n];
            }
        }
    }
}


extern void cshift0_1 (gfc_array_char *, const gfc_array_char *,
		       const GFC_INTEGER_1 *, const GFC_INTEGER_1 *);
export_proto(cshift0_1);

void
cshift0_1 (gfc_array_char *ret, const gfc_array_char *array,
	   const GFC_INTEGER_1 *pshift, const GFC_INTEGER_1 *pdim)
{
  cshift0 (ret, array, *pshift, pdim ? *pdim : 1);
}


extern void cshift0_2 (gfc_array_char *, const gfc_array_char *,
		       const GFC_INTEGER_2 *, const GFC_INTEGER_2 *);
export_proto(cshift0_2);

void
cshift0_2 (gfc_array_char *ret, const gfc_array_char *array,
	   const GFC_INTEGER_2 *pshift, const GFC_INTEGER_2 *pdim)
{
  cshift0 (ret, array, *pshift, pdim ? *pdim : 1);
}


extern void cshift0_4 (gfc_array_char *, const gfc_array_char *,
		       const GFC_INTEGER_4 *, const GFC_INTEGER_4 *);
export_proto(cshift0_4);

void
cshift0_4 (gfc_array_char *ret, const gfc_array_char *array,
	   const GFC_INTEGER_4 *pshift, const GFC_INTEGER_4 *pdim)
{
  cshift0 (ret, array, *pshift, pdim ? *pdim : 1);
}


extern void cshift0_8 (gfc_array_char *, const gfc_array_char *,
		       const GFC_INTEGER_8 *, const GFC_INTEGER_8 *);
export_proto(cshift0_8);

void
cshift0_8 (gfc_array_char *ret, const gfc_array_char *array,
	   const GFC_INTEGER_8 *pshift, const GFC_INTEGER_8 *pdim)
{
  cshift0 (ret, array, *pshift, pdim ? *pdim : 1);
}

