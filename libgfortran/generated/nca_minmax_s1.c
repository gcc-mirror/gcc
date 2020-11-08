/* Implementation of collective subroutines minmax.
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
<http://www.gnu.org/licenses/>.  */



#include "libgfortran.h"

#if defined (HAVE_GFC_UINTEGER_1)
#include <string.h>
#include "../nca/libcoarraynative.h"
#include "../nca/collective_subroutine.h"

#if 1 == 4

/* Compare wide character types, which are handled internally as
   unsigned 4-byte integers.  */
static inline int
memcmp4 (const void *a, const void *b, size_t len)
{
  const GFC_UINTEGER_4 *pa = a;
  const GFC_UINTEGER_4 *pb = b;
  while (len-- > 0)
    {
      if (*pa != *pb)
	return *pa < *pb ? -1 : 1;
      pa ++;
      pb ++;
    }
  return 0;
}

#endif
void cas_collsub_max_scalar_s1 (GFC_UINTEGER_1 *obj, int *result_image,
			int *stat, char *errmsg, index_type char_len, index_type errmsg_len);
export_proto(cas_collsub_max_scalar_s1);

void
cas_collsub_max_scalar_s1 (GFC_UINTEGER_1 *obj, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type char_len,
			   index_type errmsg_len __attribute__ ((unused)))
{
  int cbit = 0;
  int imoffset;
  GFC_UINTEGER_1 *a, *b;
  GFC_UINTEGER_1 *buffer, *this_image_buf;
  collsub_iface *ci;
  index_type type_size;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  type_size = char_len * sizeof (GFC_UINTEGER_1);
  buffer = get_collsub_buf (ci, type_size * local->total_num_images);
  this_image_buf = buffer + this_image.image_num * char_len;
  memcpy (this_image_buf, obj, type_size);

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0
    && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset * char_len;
	  if (memcmp (b, a, char_len) > 0)
	    memcpy (a, b, type_size);
	}
      collsub_sync (ci);
    }
  /* All images have to execute the same number of collsub_sync, otherwise
     some images will hang.  Here, we execute the missing ones for images
     that are not needed anymore in the main loop.  */
  for ( ; (local->total_num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    memcpy (obj, buffer, type_size);

  /* We need one barrier (it could be either before or after the collsub) that
     prevents one image from starting a new collsub before the old one has 
     finished.  */
  finish_collective_subroutine (ci);

}

void cas_collsub_min_scalar_s1 (GFC_UINTEGER_1 *obj, int *result_image,
			int *stat, char *errmsg, index_type char_len, index_type errmsg_len);
export_proto(cas_collsub_min_scalar_s1);

void
cas_collsub_min_scalar_s1 (GFC_UINTEGER_1 *obj, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type char_len,
			   index_type errmsg_len __attribute__ ((unused)))
{
  int cbit = 0;
  int imoffset;
  GFC_UINTEGER_1 *a, *b;
  GFC_UINTEGER_1 *buffer, *this_image_buf;
  collsub_iface *ci;
  index_type type_size;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  type_size = char_len * sizeof (GFC_UINTEGER_1);
  buffer = get_collsub_buf (ci, type_size * local->total_num_images);
  this_image_buf = buffer + this_image.image_num * char_len;
  memcpy (this_image_buf, obj, type_size);

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0
    && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset * char_len;
	  if (memcmp (b, a, char_len) < 0)
	    memcpy (a, b, type_size);
	}
      collsub_sync (ci);
    }
  /* All images have to execute the same number of collsub_sync, otherwise
     some images will hang.  Here, we execute the missing ones for images
     that are not needed anymore in the main loop.  */
  for ( ; (local->total_num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    memcpy (obj, buffer, type_size);

  /* We need one barrier (it could be either before or after the collsub) that
     prevents one image from starting a new collsub before the old one has 
     finished.  */
  finish_collective_subroutine (ci);

}

void cas_collsub_max_array_s1 (gfc_array_s1 * restrict array, int *result_image,
				int *stat, char *errmsg, index_type char_len,
				index_type errmsg_len);
export_proto (cas_collsub_max_array_s1);

void
cas_collsub_max_array_s1 (gfc_array_s1 * restrict array, int *result_image,
			  	     int *stat, char *errmsg, index_type char_len, 
				     index_type errmsg_len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];  /* Store byte-based strides here.  */
  index_type extent[GFC_MAX_DIMENSIONS];
  char *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  char *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  index_type type_size;
  collsub_iface *ci;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  type_size = char_len * sizeof (GFC_UINTEGER_1);
  dim = GFC_DESCRIPTOR_RANK (array);
  num_elems = 1;
  packed = true;
  span = array->span != 0 ? array->span : type_size;
  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE (array, n) * span;
      extent[n] = GFC_DESCRIPTOR_EXTENT (array, n);

      /* No-op for an empty array.  */
      if (extent[n] <= 0)
	return;

      if (num_elems != GFC_DESCRIPTOR_STRIDE (array,n))
	packed = false;

      num_elems *= extent[n];
    }

  ssize = num_elems * type_size;
  buffer = get_collsub_buf (ci, ssize * local->total_num_images);
  this_shared_ptr = buffer + this_image.image_num * ssize;

  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      char *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */

	  memcpy (dest, src, type_size);
	  dest += type_size;
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
  for (; ((this_image.image_num >> cbit) & 1) == 0
    && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  char *other_shared_ptr;  /* Points to the shared memory
				      allocated to another image.  */
	  GFC_UINTEGER_1 *a;
	  GFC_UINTEGER_1 *b;

	  other_shared_ptr = this_shared_ptr + imoffset * ssize;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = (GFC_UINTEGER_1 *) (this_shared_ptr + i * type_size);
	      b = (GFC_UINTEGER_1 *) (other_shared_ptr + i * type_size);
	      if (memcmp (b, a, char_len) > 0)
		memcpy (a, b, type_size);
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
	  char *src = buffer;
	  char *restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  memset (count, 0, sizeof (index_type) * dim);

	  while (dest)
	    {
	      memcpy (dest, src, type_size);
	      src += span;
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
void cas_collsub_min_array_s1 (gfc_array_s1 * restrict array, int *result_image,
				int *stat, char *errmsg, index_type char_len,
				index_type errmsg_len);
export_proto (cas_collsub_min_array_s1);

void
cas_collsub_min_array_s1 (gfc_array_s1 * restrict array, int *result_image,
			  	     int *stat, char *errmsg, index_type char_len, 
				     index_type errmsg_len)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];  /* Store byte-based strides here.  */
  index_type extent[GFC_MAX_DIMENSIONS];
  char *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  char *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  index_type type_size;
  collsub_iface *ci;

  STAT_ERRMSG_ENTRY_CHECK(stat, errmsg, errmsg_len);

  error_on_missing_images();

  ci = &local->ci;

  type_size = char_len * sizeof (GFC_UINTEGER_1);
  dim = GFC_DESCRIPTOR_RANK (array);
  num_elems = 1;
  packed = true;
  span = array->span != 0 ? array->span : type_size;
  for (index_type n = 0; n < dim; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE (array, n) * span;
      extent[n] = GFC_DESCRIPTOR_EXTENT (array, n);

      /* No-op for an empty array.  */
      if (extent[n] <= 0)
	return;

      if (num_elems != GFC_DESCRIPTOR_STRIDE (array,n))
	packed = false;

      num_elems *= extent[n];
    }

  ssize = num_elems * type_size;
  buffer = get_collsub_buf (ci, ssize * local->total_num_images);
  this_shared_ptr = buffer + this_image.image_num * ssize;

  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      char *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */

	  memcpy (dest, src, type_size);
	  dest += type_size;
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
  for (; ((this_image.image_num >> cbit) & 1) == 0
    && (local->total_num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  char *other_shared_ptr;  /* Points to the shared memory
				      allocated to another image.  */
	  GFC_UINTEGER_1 *a;
	  GFC_UINTEGER_1 *b;

	  other_shared_ptr = this_shared_ptr + imoffset * ssize;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = (GFC_UINTEGER_1 *) (this_shared_ptr + i * type_size);
	      b = (GFC_UINTEGER_1 *) (other_shared_ptr + i * type_size);
	      if (memcmp (b, a, char_len) < 0)
		memcpy (a, b, type_size);
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
	  char *src = buffer;
	  char *restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  memset (count, 0, sizeof (index_type) * dim);

	  while (dest)
	    {
	      memcpy (dest, src, type_size);
	      src += span;
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

#endif

