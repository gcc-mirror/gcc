/* Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

This file is part of the GNU Fortran Native Coarray Library (libnca).

Libnca is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libnca is distributed in the hope that it will be useful,
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

#include <string.h>
#include "libgfortran.h"
#include "libcoarraynative.h"
#include "collective_subroutine.h"
#include "collective_inline.h"
#include "allocator.h"

void *
get_collsub_buf (collsub_iface *ci, size_t size)
{
  void *ret;

  pthread_mutex_lock (&ci->s->mutex);
  if (size > ci->s->curr_size)
    {
      shared_free (ci->a, ci->s->collsub_buf, ci->s->curr_size);
      ci->s->collsub_buf = shared_malloc (ci->a, size);
      ci->s->curr_size = size;
    }

  ret = SHMPTR_AS (void *, ci->s->collsub_buf, ci->sm);
  pthread_mutex_unlock (&ci->s->mutex);
  return ret;
}

/* It appears as if glibc's barrier implementation does not spin (at
   least that is what I got from a quick glance at the source code),
   so performance would be improved quite a bit if we spun a few times
   here so we don't run into the futex syscall.  */

void
collsub_sync (collsub_iface *ci)
{
  //dprintf (2, "Calling collsub_sync %d times\n", ++called);
  pthread_barrier_wait (&ci->s->barrier);
}

/* assign_function is needed since we only know how to assign the type inside
   the compiler.  It should be implemented as follows:
   
     void assign_function (void *a, void *b) 
     {
       *((t *) a) = reduction_operation ((t *) a, (t *) b);
     }
   
   */

void
collsub_reduce_array (collsub_iface *ci, gfc_array_char *desc, int *result_image,
		      void (*assign_function) (void *, void *))
{
  void *buffer;
  pack_info pi;
  bool packed;
  int cbit = 0;
  int imoffset;
  index_type elem_size;
  index_type this_image_size_bytes;
  char *this_image_buf;

  packed = pack_array_prepare (&pi, desc);
  if (pi.num_elem == 0)
    return;

  elem_size = GFC_DESCRIPTOR_SIZE (desc);
  this_image_size_bytes = elem_size * pi.num_elem;

  buffer = get_collsub_buf (ci, this_image_size_bytes * local->num_images);
  this_image_buf = buffer + this_image_size_bytes * this_image.image_num;

  if (packed)
    memcpy (this_image_buf, GFC_DESCRIPTOR_DATA (desc), this_image_size_bytes);
  else
    pack_array_finish (&pi, desc, this_image_buf);

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++) 
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	/* Reduce arrays elementwise.  */
	for (size_t i = 0; i < pi.num_elem; i++) 
	  assign_function (this_image_buf + elem_size * i,
			   this_image_buf + this_image_size_bytes * imoffset + elem_size * i);
 
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);
  
  if (!result_image || *result_image == this_image.image_num)
    { 
      if (packed)
        memcpy (GFC_DESCRIPTOR_DATA (desc), buffer, this_image_size_bytes);
      else
    	unpack_array_finish(&pi, desc, buffer);
    }

  finish_collective_subroutine (ci); 
}

void
collsub_reduce_scalar (collsub_iface *ci, void *obj, index_type elem_size,
		       int *result_image,
		       void (*assign_function) (void *, void *))
{
  void *buffer;
  int cbit = 0;
  int imoffset;
  char *this_image_buf;

  buffer = get_collsub_buf (ci, elem_size * local->num_images);
  this_image_buf = buffer + elem_size * this_image.image_num;

  memcpy (this_image_buf, obj, elem_size);

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0 && (local->num_images >> cbit) != 0; cbit++) 
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->num_images)
	/* Reduce arrays elementwise.  */
	assign_function (this_image_buf, this_image_buf + elem_size*imoffset);
 
      collsub_sync (ci);
    }
  for ( ; (local->num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);
  
  if (!result_image || *result_image == this_image.image_num)
    memcpy(obj, buffer, elem_size);

  finish_collective_subroutine (ci); 
}

/* Do not use sync_all(), because the program should deadlock in the case that
 * some images are on a sync_all barrier while others are in a collective
 * subroutine.  */

void
collsub_iface_init (collsub_iface *ci, alloc_iface *ai, shared_memory *sm)
{
  pthread_barrierattr_t attr;
  shared_mem_ptr p;
  ci->s = SHARED_MEMORY_RAW_ALLOC_PTR(sm, collsub_iface_shared);

  ci->s->collsub_buf = shared_malloc(get_allocator(ai), sizeof(double)*local->num_images);
  ci->s->curr_size = sizeof(double)*local->num_images;
  ci->sm = sm;
  ci->a = get_allocator(ai);

  pthread_barrierattr_init (&attr);
  pthread_barrierattr_setpshared (&attr, PTHREAD_PROCESS_SHARED);
  pthread_barrier_init (&ci->s->barrier, &attr, local->num_images);
  pthread_barrierattr_destroy(&attr);

  initialize_shared_mutex (&ci->s->mutex);
}

void
collsub_broadcast_scalar (collsub_iface *ci, void *obj, index_type elem_size,
		       	  int source_image /* Adjusted in the wrapper.  */)
{
  void *buffer;

  buffer = get_collsub_buf (ci, elem_size);

  dprintf(2, "Source image: %d\n", source_image);

  if (source_image == this_image.image_num)
    {
      memcpy (buffer, obj, elem_size);
      collsub_sync (ci);
    }
  else
    {
      collsub_sync (ci);
      memcpy (obj, buffer, elem_size);
    }
  
  finish_collective_subroutine (ci); 
}

void
collsub_broadcast_array (collsub_iface *ci, gfc_array_char *desc, 
			 int source_image)
{
  void *buffer;
  pack_info pi;
  bool packed;
  index_type elem_size;
  index_type size_bytes;
  char *this_image_buf;

  packed = pack_array_prepare (&pi, desc);
  if (pi.num_elem == 0)
    return;

  elem_size = GFC_DESCRIPTOR_SIZE (desc);
  size_bytes = elem_size * pi.num_elem;

  buffer = get_collsub_buf (ci, size_bytes);

  if (source_image == this_image.image_num)
    {
      if (packed)
        memcpy (buffer, GFC_DESCRIPTOR_DATA (desc), size_bytes);
      else
        pack_array_finish (&pi, desc, buffer);
      collsub_sync (ci);
    }
  else 
    {
      collsub_sync (ci);
      if (packed)
	memcpy (GFC_DESCRIPTOR_DATA (desc), buffer, size_bytes);
      else
	unpack_array_finish(&pi, desc, buffer);
    }

  finish_collective_subroutine (ci); 
}

#if 0

void nca_co_broadcast (gfc_array_char *, int, int*, char *, size_t);
export_proto (nca_co_broadcast);

void
nca_co_broadcast (gfc_array_char * restrict a, int source_image,
		  int *stat, char *errmsg __attribute__ ((unused)),
		  size_t errmsg_len __attribute__ ((unused)))
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type type_size;
  index_type dim;
  index_type span;
  bool packed, empty;
  index_type num_elems;
  index_type ssize, ssize_bytes;
  char *this_shared_ptr, *other_shared_ptr;

  if (stat)
    *stat = 0;

  dim = GFC_DESCRIPTOR_RANK (a);
  type_size = GFC_DESCRIPTOR_SIZE (a);

  /* Source image, gather.  */
  if (source_image - 1 == image_num)
    {
      num_elems = 1;
      if (dim > 0)
	{
	  span = a->span != 0 ? a->span : type_size;
	  packed = true;
	  empty = false;
	  for (index_type n = 0; n < dim; n++)
	    {
	      count[n] = 0;
	      stride[n] = GFC_DESCRIPTOR_STRIDE (a, n) * span;
	      extent[n] = GFC_DESCRIPTOR_EXTENT (a, n);

	      empty = empty || extent[n] <= 0;

	      if (num_elems != GFC_DESCRIPTOR_STRIDE (a, n))
		packed = false;

	      num_elems *= extent[n];
	    }
	  ssize_bytes = num_elems * type_size;
	}
      else
	{
	  ssize_bytes = type_size;
	  packed = true;
	  empty = false;
	}

      prepare_collective_subroutine (ssize_bytes); // broadcast barrier 1
      this_shared_ptr = get_obj_ptr (image_num);
      if (packed)
	memcpy (this_shared_ptr, a->base_addr, ssize_bytes);
      else
	{
	  char *src = (char *) a->base_addr;
	  char * restrict dest = this_shared_ptr;
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
		  /* When we get to the end of a dimension, reset it
		     and increment the next dimension.  */
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
      collsub_sync (ci); /* Broadcast barrier 2.  */
    }
  else   /* Target image, scatter.  */
    {
      collsub_sync (ci);  /* Broadcast barrier 1.  */
      packed = 1;
      num_elems = 1;
      span = a->span != 0 ? a->span : type_size;

      for (index_type n = 0; n < dim; n++)
	{
	  index_type stride_n;
	  count[n] = 0;
	  stride_n = GFC_DESCRIPTOR_STRIDE (a, n);
	  stride[n] = stride_n * type_size;
	  extent[n] = GFC_DESCRIPTOR_EXTENT (a, n);
	  if (extent[n] <= 0)
	    {
	      packed = true;
	      num_elems = 0;
	      break;
	    }
	  if (num_elems != stride_n)
	    packed = false;

	  num_elems *= extent[n];
	}
      ssize = num_elems * type_size;
      prepare_collective_subroutine (ssize);  /* Broadcaset barrier 2.  */
      other_shared_ptr = get_obj_ptr (source_image - 1);
      if (packed)
	memcpy (a->base_addr, other_shared_ptr, ssize);
      else
	{
	  char *src = other_shared_ptr;
	  char * restrict dest = (char *) a->base_addr;
	  index_type stride0 = stride[0];

	  for (index_type n = 0; n < dim; n++)
	    count[n] = 0;

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
  finish_collective_subroutine (ci);  /* Broadcast barrier 3.  */
}

#endif
