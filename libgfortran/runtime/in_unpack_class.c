/* Class helper function for repacking arrays.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

extern void
internal_unpack_class (gfc_class_array_t *, gfc_class_array_t *, const size_t,
		       const int);
export_proto (internal_unpack_class);

void
internal_unpack_class (gfc_class_array_t *dest_class,
		       gfc_class_array_t *source_class, const size_t size_class,
		       const int attr)
{
#define BIT_TEST(mask, bit) (((mask) & (1U << (bit))) == (1U << (bit)))

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  index_type dsize;
  void *dest;
  const void *src;
  index_type size;
  const gfc_array_void *src_arr;
  gfc_array_void *dest_arr;
  bool len_present = BIT_TEST (attr, 0);
  gfc_vtype_generic_t *vtab;
  void (*copyfn) (const void *, void *);

  /* This check may be redundant, but do it anyway.  */
  if (!source_class || !dest_class || !source_class->_data.base_addr
      || !dest_class->_data.base_addr)
    return;

  dest_arr = (gfc_array_void *) &(dest_class->_data);
  dest = dest_arr->base_addr;
  size = GFC_DESCRIPTOR_SIZE (dest_arr);
  dim = GFC_DESCRIPTOR_RANK (dest_arr);
  dsize = 1;
  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE (dest_arr, n);
      extent[n] = GFC_DESCRIPTOR_EXTENT (dest_arr, n);
      if (extent[n] <= 0)
	return;

      if (dsize == stride[n])
	dsize *= extent[n];
      else
	dsize = 0;
    }

  src_arr = (gfc_array_void *) &source_class->_data;
  src = src_arr->base_addr;

  vtab = *(gfc_vtype_generic_t **) (((void *) source_class) + size_class
				    - (len_present ? sizeof (size_t) : 0)
				    - sizeof (void *)); /* _vptr */
  copyfn = vtab->_copy;

  if (dsize != 0)
    {
      for (index_type n = 0; n < dsize; ++n)
	{
	  copyfn (src, dest);
	  src += size;
	  dest += size;
	}
      free (src_arr->base_addr);
      return;
    }

  stride0 = stride[0] * size;

  while (dest)
    {
      /* Copy the data.  */
      copyfn (src, dest);
      /* Advance to the next element.  */
      src += size;
      dest += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  dest -= stride[n] * extent[n] * size;
	  n++;
	  if (n == dim)
	    {
	      dest = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += stride[n] * size;
	    }
	}
    }
  free (src_arr->base_addr);
}
