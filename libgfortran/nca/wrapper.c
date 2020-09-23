/* Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
#include "sync.h"
#include "lock.h"
#include "util.h"
#include "collective_subroutine.h"

static inline int
div_ru (int divident, int divisor)
{
  return (divident + divisor - 1)/divisor;
}

enum gfc_coarray_allocation_type {
  GFC_NCA_NORMAL_COARRAY = 3,
  GFC_NCA_LOCK_COARRAY,
  GFC_NCA_EVENT_COARRAY,
};

void nca_coarray_alloc (gfc_array_void *, int, int, int);
export_proto (nca_coarray_alloc);

void
nca_coarray_free (gfc_array_void *, int);
export_proto (nca_coarray_free);

int nca_coarray_this_image (int);
export_proto (nca_coarray_this_image);

int nca_coarray_num_images (int);
export_proto (nca_coarray_num_images);

void nca_coarray_sync_all (int *);
export_proto (nca_coarray_sync_all);

void nca_sync_images (size_t, int *, int*, char *, size_t);
export_proto (nca_sync_images);

void nca_lock (void *);
export_proto (nca_lock);

void nca_unlock (void *);
export_proto (nca_unlock);

void nca_collsub_reduce_array (gfc_array_char *, void (*) (void *, void *),
			       int *);
export_proto (nca_collsub_reduce_array);

void nca_collsub_reduce_scalar (void *, index_type, void (*) (void *, void *),
				int *);
export_proto (nca_collsub_reduce_scalar);

void nca_collsub_broadcast_array (gfc_array_char * restrict, int/*, int *, char *, 
			     size_t*/);
export_proto (nca_collsub_broadcast_array);

void nca_collsub_broadcast_scalar (void * restrict, size_t, int/*, int *, char *, 
			      size_t*/);
export_proto(nca_collsub_broadcast_scalar);

void
nca_coarray_alloc (gfc_array_void *desc, int elem_size, int corank,
		   int alloc_type)
{
  int i, last_rank_index;
  int num_coarray_elems, num_elems; /* Excludes the last dimension, because it
				       will have to be determined later.  */
  int extent_last_codimen;
  size_t last_lbound;
  size_t size_in_bytes;

  ensure_initialization(); /* This function might be the first one to be 
  			      called, if it is called in a constructor.  */

  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    elem_size = sizeof (pthread_mutex_t);
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    elem_size = sizeof(char); /* replace with proper type. */

  last_rank_index = GFC_DESCRIPTOR_RANK(desc) + corank -1;

  num_elems = 1;
  num_coarray_elems = 1;
  for (i = 0; i < GFC_DESCRIPTOR_RANK(desc); i++)
    num_elems *= GFC_DESCRIPTOR_EXTENT(desc, i);
  for (i = GFC_DESCRIPTOR_RANK(desc); i < last_rank_index; i++)
    {
      num_elems *= GFC_DESCRIPTOR_EXTENT(desc, i);
      num_coarray_elems *= GFC_DESCRIPTOR_EXTENT(desc, i);
    }

  extent_last_codimen = div_ru (local->num_images, num_coarray_elems);

  last_lbound = GFC_DIMENSION_LBOUND(desc->dim[last_rank_index]);
  GFC_DIMENSION_SET(desc->dim[last_rank_index], last_lbound,
		    last_lbound + extent_last_codimen - 1,
		    num_elems);

  size_in_bytes = elem_size * num_elems * extent_last_codimen;
  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    {
      lock_array *addr;
      int expected = 0;
      /* Allocate enough space for the metadata infront of the lock
	 array.  */
      addr = get_memory_by_id_zero (&local->ai, size_in_bytes
				    + sizeof (lock_array),
				    (intptr_t) desc);

      /* Use of a traditional spin lock to avoid race conditions with
	  the initization of the mutex.  We could alternatively put a
	  global lock around allocate, but that would probably be
	  slower.  */
      while (!__atomic_compare_exchange_n (&addr->owner, &expected,
					   this_image.image_num + 1,
					   false, __ATOMIC_SEQ_CST,
					   __ATOMIC_SEQ_CST));
      if (!addr->initialized++)
	{
	  for (i = 0; i < local->num_images; i++)
	    initialize_shared_mutex (&addr->arr[i]);
	}
      __atomic_store_n (&addr->owner, 0, __ATOMIC_SEQ_CST);
      desc->base_addr = &addr->arr;
    }
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    (void) 0; // TODO
  else
    desc->base_addr = get_memory_by_id (&local->ai, size_in_bytes,
					(intptr_t) desc);
  dprintf(2, "Base address of desc for image %d: %p\n", this_image.image_num + 1, desc->base_addr);
}

void
nca_coarray_free (gfc_array_void *desc, int alloc_type)
{
  int i;
  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    {
      lock_array *la;
      int expected = 0;
      la = desc->base_addr - offsetof (lock_array, arr);
      while (!__atomic_compare_exchange_n (&la->owner, &expected,
					   this_image.image_num+1,
					   false, __ATOMIC_SEQ_CST,
					   __ATOMIC_SEQ_CST));
      if (!--la->initialized)
	 {
	  /* Coarray locks can be removed and just normal
	     pthread_mutex can be used.	 */
	   for (i = 0; i < local->num_images; i++)
	     pthread_mutex_destroy (&la->arr[i]);
	 }
      __atomic_store_n (&la->owner, 0, __ATOMIC_SEQ_CST);
    }
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    (void) 0; //TODO

  free_memory_with_id (&local->ai, (intptr_t) desc);
  desc->base_addr = NULL;
}

int
nca_coarray_this_image (int distance __attribute__((unused)))
{
  return this_image.image_num + 1;
}

int
nca_coarray_num_images (int distance __attribute__((unused)))
{
  return local->num_images;
}

void
nca_coarray_sync_all (int *stat __attribute__((unused)))
{
  sync_all (&local->si);
}

void
nca_sync_images (size_t s, int *images,
			  int *stat __attribute__((unused)),
			  char *error __attribute__((unused)),
			  size_t err_size __attribute__((unused)))
{
  sync_table (&local->si, images, s);
}

void
nca_lock (void *lock)
{
  pthread_mutex_lock (lock);
}

void
nca_unlock (void *lock)
{
  pthread_mutex_unlock (lock);
}

void
nca_collsub_reduce_array (gfc_array_char *desc, void (*assign_function) (void *, void *),
			  int *result_image)
{
  collsub_reduce_array (&local->ci, desc, result_image, assign_function);
}

void
nca_collsub_reduce_scalar (void *obj, index_type elem_size,
			   void (*assign_function) (void *, void *),
			   int *result_image)
{
  collsub_reduce_scalar (&local->ci, obj, elem_size, result_image, assign_function);
}

void
nca_collsub_broadcast_array (gfc_array_char * restrict a, int source_image 
		  /* , int *stat __attribute__ ((unused)), 
		  char *errmsg __attribute__ ((unused)),
		  size_t errmsg_len __attribute__ ((unused))*/)
{
  collsub_broadcast_array (&local->ci, a, source_image - 1);
}

void
nca_collsub_broadcast_scalar (void * restrict obj, size_t size, int source_image/*,
		  int *stat __attribute__((unused)),
		   char *errmsg __attribute__ ((unused)),
		  size_t errmsg_len __attribute__ ((unused))*/)
{
  collsub_broadcast_scalar (&local->ci, obj, size, source_image - 1);
}
