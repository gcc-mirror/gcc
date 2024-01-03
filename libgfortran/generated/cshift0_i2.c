/* Helper function for cshift functions.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.
   Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

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


#if defined (HAVE_GFC_INTEGER_2)

void
cshift0_i2 (gfc_array_i2 *ret, const gfc_array_i2 *array, ptrdiff_t shift,
		     int which)
{
  /* r.* indicates the return array.  */
  index_type rstride[GFC_MAX_DIMENSIONS];
  index_type rstride0;
  index_type roffset;
  GFC_INTEGER_2 *rptr;

  /* s.* indicates the source array.  */
  index_type sstride[GFC_MAX_DIMENSIONS];
  index_type sstride0;
  index_type soffset;
  const GFC_INTEGER_2 *sptr;

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type dim;
  index_type len;
  index_type n;

  bool do_blocked;
  index_type r_ex, a_ex;

  which = which - 1;
  sstride[0] = 0;
  rstride[0] = 0;

  extent[0] = 1;
  count[0] = 0;
  n = 0;
  /* Initialized for avoiding compiler warnings.  */
  roffset = 1;
  soffset = 1;
  len = 0;

  r_ex = 1;
  a_ex = 1;

  if (which > 0)
    {
      /* Test if both ret and array are contiguous.  */
      do_blocked = true;
      dim = GFC_DESCRIPTOR_RANK (array);
      for (n = 0; n < dim; n ++)
	{
	  index_type rs, as;
	  rs = GFC_DESCRIPTOR_STRIDE (ret, n);
	  if (rs != r_ex)
	    {
	      do_blocked = false;
	      break;
	    }
	  as = GFC_DESCRIPTOR_STRIDE (array, n);
	  if (as != a_ex)
	    {
	      do_blocked = false;
	      break;
	    }
	  r_ex *= GFC_DESCRIPTOR_EXTENT (ret, n);
	  a_ex *= GFC_DESCRIPTOR_EXTENT (array, n);
	}
    }
  else
    do_blocked = false;

  n = 0;

  if (do_blocked)
    {
      /* For contiguous arrays, use the relationship that

         dimension(n1,n2,n3) :: a, b
	 b = cshift(a,sh,3)

         can be dealt with as if

	 dimension(n1*n2*n3) :: an, bn
	 bn = cshift(a,sh*n1*n2,1)

	 we can used a more blocked algorithm for dim>1.  */
      sstride[0] = 1;
      rstride[0] = 1;
      roffset = 1;
      soffset = 1;
      len = GFC_DESCRIPTOR_STRIDE(array, which)
	* GFC_DESCRIPTOR_EXTENT(array, which);      
      shift *= GFC_DESCRIPTOR_STRIDE(array, which);
      for (dim = which + 1; dim < GFC_DESCRIPTOR_RANK (array); dim++)
	{
	  count[n] = 0;
	  extent[n] = GFC_DESCRIPTOR_EXTENT(array,dim);
	  rstride[n] = GFC_DESCRIPTOR_STRIDE(ret,dim);
	  sstride[n] = GFC_DESCRIPTOR_STRIDE(array,dim);
	  n++;
	}
      dim = GFC_DESCRIPTOR_RANK (array) - which;
    }
  else
    {
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
	      n++;
	    }
	}
      if (sstride[0] == 0)
	sstride[0] = 1;
      if (rstride[0] == 0)
	rstride[0] = 1;

      dim = GFC_DESCRIPTOR_RANK (array);
    }

  rstride0 = rstride[0];
  sstride0 = sstride[0];
  rptr = ret->base_addr;
  sptr = array->base_addr;

  /* Avoid the costly modulo for trivially in-bound shifts.  */
  if (shift < 0 || shift >= len)
    {
      shift = len == 0 ? 0 : shift % (ptrdiff_t)len;
      if (shift < 0)
	shift += len;
    }

  while (rptr)
    {
      /* Do the shift for this dimension.  */

      /* If elements are contiguous, perform the operation
	 in two block moves.  */
      if (soffset == 1 && roffset == 1)
	{
	  size_t len1 = shift * sizeof (GFC_INTEGER_2);
	  size_t len2 = (len - shift) * sizeof (GFC_INTEGER_2);
	  memcpy (rptr, sptr + shift, len2);
	  memcpy (rptr + (len - shift), sptr, len1);
	}
      else
	{
	  /* Otherwise, we will have to perform the copy one element at
	     a time.  */
	  GFC_INTEGER_2 *dest = rptr;
	  const GFC_INTEGER_2 *src = &sptr[shift * soffset];

	  for (n = 0; n < len - shift; n++)
	    {
	      *dest = *src;
	      dest += roffset;
	      src += soffset;
	    }
	  for (src = sptr, n = 0; n < shift; n++)
	    {
	      *dest = *src;
	      dest += roffset;
	      src += soffset;
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
             frequently used path so probably not worth it.  */
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

  return;
}

#endif
