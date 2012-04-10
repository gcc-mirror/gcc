/* Implementation of the TRANSPOSE intrinsic
   Copyright 2003, 2005, 2006, 2007, 2009, 2012 Free Software Foundation, Inc.
   Contributed by Tobias Schlüter

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
#include <assert.h>


#if defined (HAVE_GFC_INTEGER_16)

extern void transpose_i16 (gfc_array_i16 * const restrict ret, 
	gfc_array_i16 * const restrict source);
export_proto(transpose_i16);

void
transpose_i16 (gfc_array_i16 * const restrict ret, 
	gfc_array_i16 * const restrict source)
{
  /* r.* indicates the return array.  */
  index_type rxstride, rystride;
  GFC_INTEGER_16 * restrict rptr;
  /* s.* indicates the source array.  */
  index_type sxstride, systride;
  const GFC_INTEGER_16 *sptr;

  index_type xcount, ycount;
  index_type x, y;

  assert (GFC_DESCRIPTOR_RANK (source) == 2);

  if (ret->base_addr == NULL)
    {
      assert (GFC_DESCRIPTOR_RANK (ret) == 2);
      assert (ret->dtype == source->dtype);

      GFC_DIMENSION_SET(ret->dim[0], 0, GFC_DESCRIPTOR_EXTENT(source,1) - 1,
			1);

      GFC_DIMENSION_SET(ret->dim[1], 0, GFC_DESCRIPTOR_EXTENT(source,0) - 1,
			GFC_DESCRIPTOR_EXTENT(source, 1));

      ret->base_addr = xmalloc (sizeof (GFC_INTEGER_16) * size0 ((array_t *) ret));
      ret->offset = 0;
    } else if (unlikely (compile_options.bounds_check))
    {
      index_type ret_extent, src_extent;

      ret_extent = GFC_DESCRIPTOR_EXTENT(ret,0);
      src_extent = GFC_DESCRIPTOR_EXTENT(source,1);

      if (src_extent != ret_extent)
	runtime_error ("Incorrect extent in return value of TRANSPOSE"
		       " intrinsic in dimension 1: is %ld,"
		       " should be %ld", (long int) src_extent,
		       (long int) ret_extent);

      ret_extent = GFC_DESCRIPTOR_EXTENT(ret,1);
      src_extent = GFC_DESCRIPTOR_EXTENT(source,0);

      if (src_extent != ret_extent)
	runtime_error ("Incorrect extent in return value of TRANSPOSE"
		       " intrinsic in dimension 2: is %ld,"
		       " should be %ld", (long int) src_extent,
		       (long int) ret_extent);

    }

  sxstride = GFC_DESCRIPTOR_STRIDE(source,0);
  systride = GFC_DESCRIPTOR_STRIDE(source,1);
  xcount = GFC_DESCRIPTOR_EXTENT(source,0);
  ycount = GFC_DESCRIPTOR_EXTENT(source,1);

  rxstride = GFC_DESCRIPTOR_STRIDE(ret,0);
  rystride = GFC_DESCRIPTOR_STRIDE(ret,1);

  rptr = ret->base_addr;
  sptr = source->base_addr;

  for (y=0; y < ycount; y++)
    {
      for (x=0; x < xcount; x++)
        {
          *rptr = *sptr;

          sptr += sxstride;
          rptr += rystride;
        }
        sptr += systride - (sxstride * xcount);
        rptr += rxstride - (rystride * xcount);
    }
}

#endif
