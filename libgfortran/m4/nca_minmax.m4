dnl Support macro file for intrinsic functions.
dnl Contains the generic sections of gfortran functions.
dnl This file is part of the GNU Fortran Runtime Library (libgfortran)
dnl Distributed under the GNU GPL with exception.  See COPYING for details.
dnl
`/* Implementation of collective subroutines minmax.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>.

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
<http://www.gnu.org/licenses/>.  */'

include(iparm.m4)dnl
define(SCALAR_FUNCTION,`void cas_collsub_'$1`_scalar_'rtype_code` ('rtype_name` *obj, int *result_image,
			int *stat, char *errmsg, index_type errmsg_len);
export_proto(cas_collsub_'$1`_scalar_'rtype_code`);

void
cas_collsub_'$1`_scalar_'rtype_code` ('rtype_name` *obj, int *result_image,
			      	       int *stat, char *errmsg, index_type errmsg_len)
{
  int cbit = 0;
  int imoffset;
  'rtype_name` *a, *b;
  'rtype_name` *buffer, *this_image_buf;
  collsub_iface *ci;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  buffer = get_collsub_buf (ci, sizeof('rtype_name`) * local->total_num_images);
  this_image_buf = buffer + this_image.image_num;
  *this_image_buf = *obj;

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset;
	  '$2`
	}
      collsub_sync (ci);
    }
  for ( ; (local->total_num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    *obj = *buffer;

  finish_collective_subroutine (ci);

}

')dnl
define(ARRAY_FUNCTION,dnl
`void cas_collsub_'$1`_array_'rtype_code` ('rtype` * restrict array, int *result_image,
				      int *stat, char *errmsg, index_type errmsg_len);
export_proto (cas_collsub_'$1`_array_'rtype_code`);

void
cas_collsub_'$1`_array_'rtype_code` ('rtype` * restrict array, int *result_image,
			   	      int *stat, char *errmsg, index_type errmsg_len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  'rtype_name` *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  'rtype_name` *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  collsub_iface *ci;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  dim = GFC_DESCRIPTOR_RANK (array);
  ssize = sizeof ('rtype_name`);
  packed = true;
  span = array->span != 0 ? array->span : (index_type) sizeof ('rtype_name`);
  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE (array, n) * span;
      extent[n] = GFC_DESCRIPTOR_EXTENT (array, n);

      /* No-op for an empty array.  */
      if (extent[n] <= 0)
	return;

      if (ssize != stride[n])
	packed = false;

      ssize *= extent[n];
    }

  num_elems = ssize / sizeof ('rtype_name`);

  buffer = get_collsub_buf (ci, ssize * local->total_num_images);
  this_shared_ptr = buffer + this_image.image_num * num_elems;
  
  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      'rtype_name` *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */
	  *(dest++) = *(('rtype_name` *) src);
	  src += stride0;
	  count[0] ++;
	  /* Advance to the next source element.  */
	  for (index_type n = 0; count[n] == extent[n] ; )
	    {
	      /* When we get to the end of a dimension, reset it and increment
		 the next dimension.  */
	      count[n] = 0;
	      src -= stride[n] * extent[n];
	      n++;
	      if (n == dim)
		{
		  src = NULL;
		  break;
		}
	      else
		{
		  count[n]++;
		  src += stride[n];
		}
	    }
	}
    }

  collsub_sync (ci);

  /* Reduce the array to image zero. Here the general scheme:

      abababababab
      a_b_a_b_a_b_
      a___b___a___
      a_______b___
      r___________
  */
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  'rtype_name` * other_shared_ptr;  /* Points to the shared memory
						allocated to another image.  */
	  'rtype_name` *a;
	  'rtype_name` *b;

	  other_shared_ptr = this_shared_ptr + num_elems * imoffset;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = this_shared_ptr + i;
	      b = other_shared_ptr + i;
	      '$2`
	    }
	}
      collsub_sync (ci);
    }
  for ( ; (local->total_num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    {
      if (packed)
	memcpy (array->base_addr, buffer, ssize);
      else
	{
	  'rtype_name` *src = buffer;
	  char * restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  memset (count, 0, sizeof(index_type) * dim);

	  while (dest)
	    {
	      *(('rtype_name` * ) dest) =  *src++;
	      dest += stride0;
	      count[0] ++;
	      for (index_type n = 0; count[n] == extent[n] ;)
	        {
	      	  /* When we get to the end of a dimension, reset it and increment
		     the next dimension.  */
	      	   count[n] = 0;
	      	   dest -= stride[n] * extent[n];
	      	   n++;
	      	   if (n == dim)
		     {
		       dest = NULL;
		       break;
		     }
	      	   else
		     {
		       count[n]++;
		       dest += stride[n];
		     }
		}
	    }
	}
    }
    finish_collective_subroutine (ci);
}
')
`#include "libgfortran.h"

#if defined (HAVE_'rtype_name`)'
#include <string.h>
#include "../nca/libcoarraynative.h"
#include "../nca/collective_subroutine.h"

SCALAR_FUNCTION(`max',`if (*b > *a)
	    *a = *b;')dnl
SCALAR_FUNCTION(`min',`if (*b < *a)
	    *a = *b;')dnl
SCALAR_FUNCTION(`sum',`*a += *b;')dnl
ARRAY_FUNCTION(`max',`if (*b > *a)
		*a = *b;')dnl
ARRAY_FUNCTION(`min',`if (*b < *a)
		*a = *b;')dnl
ARRAY_FUNCTION(`sum',`*a += *b;')dnl
`
#endif
'
