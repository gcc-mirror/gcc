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

#if defined (HAVE_GFC_REAL_4)
#include <string.h>
#include "../nca/libcoarraynative.h"
#include "../nca/collective_subroutine.h"
#include "../nca/collective_inline.h"

void nca_collsub_max_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			int *stat, char *errmsg, index_type errmsg_len);
export_proto(nca_collsub_max_scalar_r4);

void
nca_collsub_max_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  int cbit = 0;
  int imoffset;
  GFC_REAL_4 *a, *b;
  GFC_REAL_4 *buffer, *this_image_buf;
  collsub_iface *ci;

  ci = &local->ci;

  buffer = get_collsub_buf (ci, sizeof(GFC_REAL_4) * local->num_images);
  this_image_buf = buffer + this_image.image_num;
  *this_image_buf = *obj;

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset;
	  if (*b > *a)
	    *a = *b;
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    *obj = *buffer;

  finish_collective_subroutine (ci);

}

void nca_collsub_min_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			int *stat, char *errmsg, index_type errmsg_len);
export_proto(nca_collsub_min_scalar_r4);

void
nca_collsub_min_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  int cbit = 0;
  int imoffset;
  GFC_REAL_4 *a, *b;
  GFC_REAL_4 *buffer, *this_image_buf;
  collsub_iface *ci;

  ci = &local->ci;

  buffer = get_collsub_buf (ci, sizeof(GFC_REAL_4) * local->num_images);
  this_image_buf = buffer + this_image.image_num;
  *this_image_buf = *obj;

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset;
	  if (*b < *a)
	    *a = *b;
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    *obj = *buffer;

  finish_collective_subroutine (ci);

}

void nca_collsub_sum_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			int *stat, char *errmsg, index_type errmsg_len);
export_proto(nca_collsub_sum_scalar_r4);

void
nca_collsub_sum_scalar_r4 (GFC_REAL_4 *obj, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  int cbit = 0;
  int imoffset;
  GFC_REAL_4 *a, *b;
  GFC_REAL_4 *buffer, *this_image_buf;
  collsub_iface *ci;

  ci = &local->ci;

  buffer = get_collsub_buf (ci, sizeof(GFC_REAL_4) * local->num_images);
  this_image_buf = buffer + this_image.image_num;
  *this_image_buf = *obj;

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  a = this_image_buf;
	  b = this_image_buf + imoffset;
	  *a += *b;
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    *obj = *buffer;

  finish_collective_subroutine (ci);

}

void nca_collsub_max_array_r4 (gfc_array_r4 * restrict array, int *result_image,
				      int *stat, char *errmsg, index_type errmsg_len);
export_proto (nca_collsub_max_array_r4);

void
nca_collsub_max_array_r4 (gfc_array_r4 * restrict array, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  GFC_REAL_4 *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  GFC_REAL_4 *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  collsub_iface *ci;

  ci = &local->ci;

  dim = GFC_DESCRIPTOR_RANK (array);
  ssize = sizeof (GFC_REAL_4);
  packed = true;
  span = array->span != 0 ? array->span : (index_type) sizeof (GFC_REAL_4);
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

  num_elems = ssize / sizeof (GFC_REAL_4);

  buffer = get_collsub_buf (ci, ssize * local->num_images);
  this_shared_ptr = buffer + this_image.image_num * num_elems;
  
  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      GFC_REAL_4 *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */
	  *(dest++) = *((GFC_REAL_4 *) src);
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
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  GFC_REAL_4 * other_shared_ptr;  /* Points to the shared memory
						allocated to another image.  */
	  GFC_REAL_4 *a;
	  GFC_REAL_4 *b;

	  other_shared_ptr = this_shared_ptr + num_elems * imoffset;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = this_shared_ptr + i;
	      b = other_shared_ptr + i;
	      if (*b > *a)
		*a = *b;
	    }
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    {
      if (packed)
	memcpy (array->base_addr, buffer, ssize);
      else
	{
	  GFC_REAL_4 *src = buffer;
	  char * restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  for (index_type n = 0; n < dim; n++)
	    count[n] = 0;

	  while (dest)
	    {
	      *((GFC_REAL_4 * ) dest) =  *src++;
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
void nca_collsub_min_array_r4 (gfc_array_r4 * restrict array, int *result_image,
				      int *stat, char *errmsg, index_type errmsg_len);
export_proto (nca_collsub_min_array_r4);

void
nca_collsub_min_array_r4 (gfc_array_r4 * restrict array, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  GFC_REAL_4 *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  GFC_REAL_4 *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  collsub_iface *ci;

  ci = &local->ci;

  dim = GFC_DESCRIPTOR_RANK (array);
  ssize = sizeof (GFC_REAL_4);
  packed = true;
  span = array->span != 0 ? array->span : (index_type) sizeof (GFC_REAL_4);
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

  num_elems = ssize / sizeof (GFC_REAL_4);

  buffer = get_collsub_buf (ci, ssize * local->num_images);
  this_shared_ptr = buffer + this_image.image_num * num_elems;
  
  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      GFC_REAL_4 *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */
	  *(dest++) = *((GFC_REAL_4 *) src);
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
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  GFC_REAL_4 * other_shared_ptr;  /* Points to the shared memory
						allocated to another image.  */
	  GFC_REAL_4 *a;
	  GFC_REAL_4 *b;

	  other_shared_ptr = this_shared_ptr + num_elems * imoffset;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = this_shared_ptr + i;
	      b = other_shared_ptr + i;
	      if (*b < *a)
		*a = *b;
	    }
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    {
      if (packed)
	memcpy (array->base_addr, buffer, ssize);
      else
	{
	  GFC_REAL_4 *src = buffer;
	  char * restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  for (index_type n = 0; n < dim; n++)
	    count[n] = 0;

	  while (dest)
	    {
	      *((GFC_REAL_4 * ) dest) =  *src++;
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
void nca_collsub_sum_array_r4 (gfc_array_r4 * restrict array, int *result_image,
				      int *stat, char *errmsg, index_type errmsg_len);
export_proto (nca_collsub_sum_array_r4);

void
nca_collsub_sum_array_r4 (gfc_array_r4 * restrict array, int *result_image,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   index_type errmsg_len __attribute__ ((unused)))
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  GFC_REAL_4 *this_shared_ptr;  /* Points to the shared memory allocated to this image.  */
  GFC_REAL_4 *buffer;
  index_type dim;
  bool packed;
  index_type span;
  index_type ssize, num_elems;
  int cbit = 0;
  int imoffset;
  collsub_iface *ci;

  ci = &local->ci;

  dim = GFC_DESCRIPTOR_RANK (array);
  ssize = sizeof (GFC_REAL_4);
  packed = true;
  span = array->span != 0 ? array->span : (index_type) sizeof (GFC_REAL_4);
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

  num_elems = ssize / sizeof (GFC_REAL_4);

  buffer = get_collsub_buf (ci, ssize * local->num_images);
  this_shared_ptr = buffer + this_image.image_num * num_elems;
  
  if (packed)
    memcpy (this_shared_ptr, array->base_addr, ssize);
  else
    {
      char *src = (char *) array->base_addr;
      GFC_REAL_4 *restrict dest = this_shared_ptr;
      index_type stride0 = stride[0];

      while (src)
	{
	  /* Copy the data.  */
	  *(dest++) = *((GFC_REAL_4 *) src);
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
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	{
	  GFC_REAL_4 * other_shared_ptr;  /* Points to the shared memory
						allocated to another image.  */
	  GFC_REAL_4 *a;
	  GFC_REAL_4 *b;

	  other_shared_ptr = this_shared_ptr + num_elems * imoffset;
	  for (index_type i = 0; i < num_elems; i++)
	    {
	      a = this_shared_ptr + i;
	      b = other_shared_ptr + i;
	      *a += *b;
	    }
	}
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1) == this_image.image_num)
    {
      if (packed)
	memcpy (array->base_addr, buffer, ssize);
      else
	{
	  GFC_REAL_4 *src = buffer;
	  char * restrict dest = (char *) array->base_addr;
	  index_type stride0 = stride[0];

	  for (index_type n = 0; n < dim; n++)
	    count[n] = 0;

	  while (dest)
	    {
	      *((GFC_REAL_4 * ) dest) =  *src++;
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

