/* Class specific helper function for repacking arrays.
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
internal_pack_class (gfc_class_array_t *, gfc_class_array_t *, const size_t,
		     const int);
export_proto (internal_pack_class);

/* attr is a bitfield.  The bits in use are:
   0 - _len is present.
 */
void
internal_pack_class (gfc_class_array_t *dest_class,
		     gfc_class_array_t *source_class, const size_t size_class,
		     const int attr)
{
#define BIT_TEST(mask, bit) (((mask) & (1U << (bit))) == (1U << (bit)))

  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0;
  index_type dim;
  index_type ssize;
  index_type dest_stride;
  index_type n;
  const void *src;
  void *dest;
  int packed;
  index_type size;
  gfc_array_void *source_arr;
  gfc_array_void *dest_arr;
  size_t dest_offset;
  bool len_present = BIT_TEST (attr, 0);
  gfc_vtype_generic_t *vtab;
  void (*copyfn) (const void *, void *);

  /* Always make sure the dest is initialized.  */
  memcpy (dest_class, source_class, size_class);
  if (source_class->_data.base_addr == NULL)
    return;

  source_arr = (gfc_array_void *) &(source_class->_data);
  size = GFC_DESCRIPTOR_SIZE (source_arr);
  dim = GFC_DESCRIPTOR_RANK (source_arr);
  ssize = 1;
  packed = 1;
  for (n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE (source_arr, n);
      extent[n] = GFC_DESCRIPTOR_EXTENT (source_arr, n);
      if (extent[n] <= 0)
	{
	  /* Do nothing.  */
	  packed = 1;
	  break;
	}

      if (ssize != stride[n])
	packed = 0;

      ssize *= extent[n];
    }

  /* When the data is packed already, nothing needs to be done and unpack, will
     quit immediately, because _data is identical and nothing needs to be done.
   */
  if (packed)
    return;

  /* Allocate storage for the destination.  */
  dest_arr = (gfc_array_void *) &dest_class->_data;
  dest_stride = 1;
  dest_offset = 0;
  for (n = 0; n < dim; ++n)
    {
      GFC_DESCRIPTOR_LBOUND (dest_arr, n) = 1;
      GFC_DESCRIPTOR_UBOUND (dest_arr, n) = extent[n];
      GFC_DESCRIPTOR_STRIDE (dest_arr, n) = dest_stride;
      dest_offset -= dest_stride * 1 /* GFC_DESCRIPTOR_LBOUND (dest_arr, n) */;
      dest_stride *= GFC_DESCRIPTOR_EXTENT (dest_arr, n);
    }
  dest_arr->offset = dest_offset;
  dest_arr->base_addr = xmallocarray (ssize, size);
  dest = (void *) dest_arr->base_addr;
  src = source_arr->base_addr;
  stride0 = stride[0] * size;
  /* Can not use the dimension here, because the class may be allocated for
     a higher dimensional array, but only a smaller amount is present.  */
  vtab = *(gfc_vtype_generic_t **) (((void *) source_class) + size_class
				    - (len_present ? sizeof (size_t) : 0)
				    - sizeof (void *)); /* _vptr */
  copyfn = vtab->_copy;

  while (src)
    {
      /* Copy the data.  */
      copyfn (src, dest);
      /* Advance to the next element.  */
      dest += size;
      src += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      n = 0;
      while (count[n] == extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  src -= stride[n] * extent[n] * size;
	  n++;
	  if (n == dim)
	    {
	      src = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      src += stride[n] * size;
	    }
	}
    }
}
