/* Deep copy support for allocatable components in derived types.
   Copyright (C) 2025 Free Software Foundation, Inc.

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

/* Runtime helper for deep copying allocatable array components when the
   element type contains nested allocatable components.  The front end handles
   allocation and deallocation; this helper performs element-wise copies using
   the compiler-generated element copier so that recursion takes place at
   runtime.  */

static inline size_t
descriptor_elem_size (gfc_array_void *desc)
{
  size_t size = GFC_DESCRIPTOR_SIZE (desc);
  return size == 0 ? 1 : size;
}

void
cfi_deep_copy_array (gfc_array_void *dest, gfc_array_void *src,
		     void (*copy_element) (void *, void *))
{
  int rank;
  size_t src_elem_size;
  size_t dest_elem_size;
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type src_stride_bytes[GFC_MAX_DIMENSIONS];
  index_type dest_stride_bytes[GFC_MAX_DIMENSIONS];
  index_type count[GFC_MAX_DIMENSIONS];
  char *src_ptr;
  char *dest_ptr;

  if (src == NULL || dest == NULL)
    return;

  if (GFC_DESCRIPTOR_DATA (src) == NULL)
    {
      if (GFC_DESCRIPTOR_DATA (dest) != NULL)
        internal_error (NULL, "cfi_deep_copy_array: destination must be "
                              "deallocated when source is not allocated");
      return;
    }

  if (GFC_DESCRIPTOR_DATA (dest) == NULL)
    internal_error (NULL, "cfi_deep_copy_array: destination not allocated");

  rank = GFC_DESCRIPTOR_RANK (src);
  src_elem_size = descriptor_elem_size (src);
  dest_elem_size = descriptor_elem_size (dest);

  if (rank <= 0)
    {
      memcpy (GFC_DESCRIPTOR_DATA (dest), GFC_DESCRIPTOR_DATA (src),
              src_elem_size);
      if (copy_element != NULL)
        copy_element (GFC_DESCRIPTOR_DATA (dest),
                      GFC_DESCRIPTOR_DATA (src));
      return;
    }

  for (int dim = 0; dim < rank; dim++)
    {
      extent[dim] = GFC_DESCRIPTOR_EXTENT (src, dim);
      if (extent[dim] <= 0)
        return;

      src_stride_bytes[dim]
        = GFC_DESCRIPTOR_STRIDE (src, dim) * src_elem_size;
      dest_stride_bytes[dim]
        = GFC_DESCRIPTOR_STRIDE (dest, dim) * dest_elem_size;
      count[dim] = 0;
    }

  src_ptr = (char *) GFC_DESCRIPTOR_DATA (src);
  dest_ptr = (char *) GFC_DESCRIPTOR_DATA (dest);

  while (true)
    {
      memcpy (dest_ptr, src_ptr, src_elem_size);
      if (copy_element != NULL)
        copy_element (dest_ptr, src_ptr);

      dest_ptr += dest_stride_bytes[0];
      src_ptr += src_stride_bytes[0];
      count[0]++;

      int dim = 0;
      while (count[dim] == extent[dim])
        {
          count[dim] = 0;
          dest_ptr -= dest_stride_bytes[dim] * extent[dim];
          src_ptr -= src_stride_bytes[dim] * extent[dim];
          dim++;
          if (dim == rank)
            return;
          count[dim]++;
          dest_ptr += dest_stride_bytes[dim];
          src_ptr += src_stride_bytes[dim];
        }
    }
}

export_proto(cfi_deep_copy_array);
