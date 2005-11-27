`/* Implementation of the EOSHIFT intrinsic
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
#include "libgfortran.h"'
include(iparm.m4)dnl

`#if defined (HAVE_'atype_name`)'

static void
eoshift3 (gfc_array_char * const restrict ret, 
	const gfc_array_char * const restrict array, 
	const atype * const restrict h,
	const gfc_array_char * const restrict bound, 
	const atype_name * const restrict pwhich,
	index_type size, char filler)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type roffset;
  char *rptr;
  char *dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type soffset;
  const char *sptr;
  const char *src;
`  /* h.* indicates the shift array.  */'
  index_type hstride[GFC_MAX_DIMENSIONS];
  index_type hstride0;
  const atype_name *hptr;
  /* b.* indicates the bound array.  */
  index_type bstride[GFC_MAX_DIMENSIONS];
  index_type bstride0;
  const char *bptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dim;
  index_type len;
  index_type n;
  int which;
  atype_name sh;
  atype_name delta;

  /* The compiler cannot figure out that these are set, initialize
     them to avoid warnings.  */
  len = 0;
  soffset = 0;
  roffset = 0;

  if (pwhich)
    which = *pwhich - 1;
  else
    which = 0;

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


  extent[0] = 1;
  count[0] = 0;
  n = 0;
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

          hstride[n] = h->dim[n].stride;
          if (bound)
            bstride[n] = bound->dim[n].stride * size;
          else
            bstride[n] = 0;
          n++;
        }
    }
  if (sstride[0] == 0)
    sstride[0] = size;
  if (rstride[0] == 0)
    rstride[0] = size;
  if (hstride[0] == 0)
    hstride[0] = 1;
  if (bound && bstride[0] == 0)
    bstride[0] = size;

  dim = GFC_DESCRIPTOR_RANK (array);
  rstride0 = rstride[0];
  sstride0 = sstride[0];
  hstride0 = hstride[0];
  bstride0 = bstride[0];
  rptr = ret->data;
  sptr = array->data;
  hptr = h->data;
  if (bound)
    bptr = bound->data;
  else
    bptr = NULL;

  while (rptr)
    {
`      /* Do the shift for this dimension.  */'
      sh = *hptr;
      if (( sh >= 0 ? sh : -sh ) > len)
	{
	  delta = len;
	  sh = len;
	}
      else
	delta = (sh >= 0) ? sh: -sh;

      if (sh > 0)
        {
          src = &sptr[delta * soffset];
          dest = rptr;
        }
      else
        {
          src = sptr;
          dest = &rptr[delta * roffset];
        }
      for (n = 0; n < len - delta; n++)
        {
          memcpy (dest, src, size);
          dest += roffset;
          src += soffset;
        }
      if (sh < 0)
        dest = rptr;
      n = delta;

      if (bptr)
	while (n--)
	  {
	    memcpy (dest, bptr, size);
	    dest += roffset;
	  }
      else
	while (n--)
	  {
	    memset (dest, filler, size);
	    dest += roffset;
	  }

      /* Advance to the next section.  */
      rptr += rstride0;
      sptr += sstride0;
      hptr += hstride0;
      bptr += bstride0;
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
	  hptr -= hstride[n] * extent[n];
          bptr -= bstride[n] * extent[n];
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
	      hptr += hstride[n];
              bptr += bstride[n];
            }
        }
    }
}

extern void eoshift3_`'atype_kind (gfc_array_char * const restrict, 
	const gfc_array_char * const restrict,
	const atype * const restrict, 
	const gfc_array_char * const restrict,
	const atype_name *);
export_proto(eoshift3_`'atype_kind);

void
eoshift3_`'atype_kind (gfc_array_char * const restrict ret, 
	const gfc_array_char * const restrict array,
	const atype * const restrict h, 
	const gfc_array_char * const restrict bound,
	const atype_name * const restrict pwhich)
{
  eoshift3 (ret, array, h, bound, pwhich, GFC_DESCRIPTOR_SIZE (array), 0);
}

extern void eoshift3_`'atype_kind`'_char (gfc_array_char * const restrict, 
	GFC_INTEGER_4,
	const gfc_array_char * const restrict,
	const atype * const restrict,
	const gfc_array_char * const restrict,
	const atype_name * const restrict, 
	GFC_INTEGER_4, GFC_INTEGER_4);
export_proto(eoshift3_`'atype_kind`'_char);

void
eoshift3_`'atype_kind`'_char (gfc_array_char * const restrict ret,
	GFC_INTEGER_4 ret_length __attribute__((unused)),
	const gfc_array_char * const restrict array, 
	const atype *  const restrict h,
	const gfc_array_char * const restrict bound,
	const atype_name * const restrict pwhich,
	GFC_INTEGER_4 array_length,
	GFC_INTEGER_4 bound_length __attribute__((unused)))
{
  eoshift3 (ret, array, h, bound, pwhich, array_length, ' ');
}

#endif
