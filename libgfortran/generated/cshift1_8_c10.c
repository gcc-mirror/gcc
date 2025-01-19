/* Implementation of the CSHIFT intrinsic.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>

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
#include <string.h>

#if defined (HAVE_GFC_COMPLEX_10) && defined (HAVE_GFC_INTEGER_8)

void
cshift1_8_c10 (gfc_array_c10 * const restrict ret,
		const gfc_array_c10 * const restrict array,
		const gfc_array_i8 * const restrict h,
		const GFC_INTEGER_8 * const restrict pwhich)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type roffset;
  GFC_COMPLEX_10 *rptr;
  GFC_COMPLEX_10 *dest;
  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type soffset;
  const GFC_COMPLEX_10 *sptr;
  const GFC_COMPLEX_10 *src;
  /* h.* indicates the shift array.  */
  index_type hstride[GFC_MAX_DIMENSIONS];
  index_type hstride0;
  const GFC_INTEGER_8 *hptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type rs_ex[GFC_MAX_DIMENSIONS];
  index_type ss_ex[GFC_MAX_DIMENSIONS];
  index_type hs_ex[GFC_MAX_DIMENSIONS];

  index_type dim;
  index_type len;
  index_type n;
  int which;
  GFC_INTEGER_8 sh;

  /* Bounds checking etc is already done by the caller.  */

  if (pwhich)
    which = *pwhich - 1;
  else
    which = 0;

  extent[0] = 1;
  count[0] = 0;
  n = 0;

  /* Initialized for avoiding compiler warnings.  */
  roffset = 1;
  soffset = 1;
  len = 0;

  for (dim = 0; dim < GFC_DESCRIPTOR_RANK (array); dim++)
    {
      if (dim == which)
        {
          roffset = GFC_DESCRIPTOR_STRIDE(ret,dim);
          if (roffset == 0)
            roffset = 1;
          soffset = GFC_DESCRIPTOR_STRIDE(array,dim);
          if (soffset == 0)
            soffset = 1;
          len = GFC_DESCRIPTOR_EXTENT(array,dim);
        }
      else
        {
          count[n] = 0;
          extent[n] = GFC_DESCRIPTOR_EXTENT(array,dim);
          rstride[n] = GFC_DESCRIPTOR_STRIDE(ret,dim);
          sstride[n] = GFC_DESCRIPTOR_STRIDE(array,dim);
          hstride[n] = GFC_DESCRIPTOR_STRIDE(h,n);
	  rs_ex[n] = rstride[n] * extent[n];
	  ss_ex[n] = sstride[n] * extent[n];
	  hs_ex[n] = hstride[n] * extent[n];
          n++;
        }
    }
  if (sstride[0] == 0)
    sstride[0] = 1;
  if (rstride[0] == 0)
    rstride[0] = 1;
  if (hstride[0] == 0)
    hstride[0] = 1;

  dim = GFC_DESCRIPTOR_RANK (array);
  rstride0 = rstride[0];
  sstride0 = sstride[0];
  hstride0 = hstride[0];
  rptr = ret->base_addr;
  sptr = array->base_addr;
  hptr = h->base_addr;

  while (rptr)
    {
      /* Do the shift for this dimension.  */
      sh = *hptr;
      /* Normal case should be -len < sh < len; try to
         avoid the expensive remainder operation if possible.  */
      if (sh < 0)
        sh += len;
      if (unlikely(sh >= len || sh < 0))
	{
 	  sh = sh % len;
	  if (sh < 0)
            sh += len;
	}
      src = &sptr[sh * soffset];
      dest = rptr;
      if (soffset == 1 && roffset == 1)
	{
	  size_t len1 = sh * sizeof (GFC_COMPLEX_10);
	  size_t len2 = (len - sh) * sizeof (GFC_COMPLEX_10);
	  memcpy (rptr, sptr + sh, len2);
	  memcpy (rptr + (len - sh), sptr, len1);
	}
      else
        {
	  for (n = 0; n < len - sh; n++)
	    {
	      *dest = *src;
	      dest += roffset;
	      src += soffset;
	    }
	  for (src = sptr, n = 0; n < sh; n++)
	    {
	      *dest = *src;
	      dest += roffset;
	      src += soffset;
	    }
	}

      /* Advance to the next section.  */
      rptr += rstride0;
      sptr += sstride0;
      hptr += hstride0;
      count[0]++;
      n = 0;
      while (count[n] == extent[n])
        {
          /* When we get to the end of a dimension, reset it and increment
             the next dimension.  */
          count[n] = 0;
          rptr -= rs_ex[n];
          sptr -= ss_ex[n];
	  hptr -= hs_ex[n];
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
            }
        }
    }
}

#endif
