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

#include "libgfortran.h"
#include "libcoarraynative.h"
#include "lock.h"

#include <string.h>

static inline int
div_ru (int divident, int divisor)
{
  return (divident + divisor - 1) / divisor;
}

/* Need to keep this in sync with
   trans-array.h:gfc_coarray_allocation_type.  */

enum gfc_coarray_allocation_type
{
  GFC_NCA_NORMAL_COARRAY = 1,
  GFC_NCA_LOCK_COARRAY,
  GFC_NCA_EVENT_COARRAY,
};

void cas_coarray_alloc (gfc_array_void *, size_t, int, int);
export_proto (cas_coarray_alloc);

void cas_coarray_alloc_chk (gfc_array_void *, size_t, int, int, int *,
			    char *, size_t);
export_proto (cas_coarray_alloc_chk);

void cas_coarray_free (gfc_array_void *, int);
export_proto (cas_coarray_free);

int cas_coarray_this_image (int);
export_proto (cas_coarray_this_image);

int cas_coarray_num_images (int);
export_proto (cas_coarray_num_images);

void cas_coarray_sync_all (int *);
export_proto (cas_coarray_sync_all);

void cas_sync_images (int, int *, int *, char *, size_t);
export_proto (cas_sync_images);

void cas_lock (void *);
export_proto (cas_lock);

void cas_unlock (void *);
export_proto (cas_unlock);

void cas_collsub_reduce_array (gfc_array_char *, void (*) (void *, void *),
			       int *, int *, char *, size_t);
export_proto (cas_collsub_reduce_array);

void cas_collsub_reduce_scalar (void *, index_type, void (*) (void *, void *),
				int *, int *, char *, size_t);
export_proto (cas_collsub_reduce_scalar);

void cas_collsub_broadcast_array (gfc_array_char *restrict, int, int *, char *,
				  size_t);
export_proto (cas_collsub_broadcast_array);

void cas_collsub_broadcast_scalar (void *restrict, size_t, int, int *, char *,
				   size_t);
export_proto (cas_collsub_broadcast_scalar);

static void
cas_coarray_alloc_work (gfc_array_void *desc, size_t elem_size, int corank,
			int alloc_type)
{
  int i, last_rank_index;
  int num_coarray_elems, num_elems; /* Excludes the last dimension, because it
				       will have to be determined later.  */
  int extent_last_codimen;
  size_t last_lbound;
  size_t size_in_bytes;

  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    elem_size = sizeof (pthread_mutex_t);
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    elem_size = sizeof (char); /* replace with proper type. */

  last_rank_index = GFC_DESCRIPTOR_RANK (desc) + corank - 1;

  num_elems = 1;
  num_coarray_elems = 1;
  for (i = 0; i < GFC_DESCRIPTOR_RANK (desc); i++)
    num_elems *= GFC_DESCRIPTOR_EXTENT (desc, i);
  for (i = GFC_DESCRIPTOR_RANK (desc); i < last_rank_index; i++)
    {
      num_elems *= GFC_DESCRIPTOR_EXTENT (desc, i);
      num_coarray_elems *= GFC_DESCRIPTOR_EXTENT (desc, i);
    }

  extent_last_codimen = div_ru (local->total_num_images, num_coarray_elems);

  last_lbound = GFC_DIMENSION_LBOUND (desc->dim[last_rank_index]);
  GFC_DIMENSION_SET (desc->dim[last_rank_index], last_lbound,
		     last_lbound + extent_last_codimen - 1, num_elems);

  size_in_bytes = elem_size * num_elems * extent_last_codimen;
  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    {
      lock_array *addr;
      int expected = 0;
      /* Allocate enough space for the metadata infront of the lock
	 array.  */
      addr = get_memory_by_id_zero (
	  &local->ai, size_in_bytes + sizeof (lock_array), (intptr_t)desc);

      /* Use of a traditional spin lock to avoid race conditions with
	  the initization of the mutex.  We could alternatively put a
	  global lock around allocate, but that would probably be
	  slower.  */
      while (!__atomic_compare_exchange_n (&addr->owner, &expected,
					   this_image.image_num + 1, false,
					   __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
	;
      if (!addr->initialized++)
	{
	  for (i = 0; i < local->total_num_images; i++)
	    initialize_shared_mutex (&addr->arr[i]);
	}
      __atomic_store_n (&addr->owner, 0, __ATOMIC_SEQ_CST);
      desc->base_addr = &addr->arr;
    }
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    (void)0; // TODO
  else
    desc->base_addr =
      get_memory_by_id (&local->ai, size_in_bytes, (intptr_t) desc);
}

void
cas_coarray_alloc (gfc_array_void *desc, size_t elem_size, int corank,
		   int alloc_type)
{
  ensure_initialization (); /* This function might be the first one to be
			       called, if it is called in a constructor.  */
  cas_coarray_alloc_work (desc, elem_size, corank, alloc_type);
}

void
cas_coarray_alloc_chk (gfc_array_void *desc, size_t elem_size, int corank,
		       int alloc_type, int *status, char *errmsg,
		       size_t errmsg_len)
{
  STAT_ERRMSG_ENTRY_CHECK (status, errmsg, errmsg_len);
  if (unlikely(GFC_DESCRIPTOR_DATA (desc) != NULL))
    {
      if (status == NULL)
	{
	  fprintf (stderr,"Image %d: Attempting to allocate already allocated "
		   "variable at %p %p\n", this_image.image_num + 1, (void *) desc,
		   desc->base_addr);
	  exit (1);
	}
      else
	{
	  *status = LIBERROR_ALLOCATION;
	  if (errmsg)
	    {
	      size_t errmsg_written_bytes;
	      errmsg_written_bytes
		= snprintf (errmsg, errmsg_len, "Attempting to allocate already "
			    "allocated variable");
	      if (errmsg_written_bytes > errmsg_len - 1)
		errmsg_written_bytes = errmsg_len - 1;
	      memset (errmsg + errmsg_written_bytes, ' ',
		      errmsg_len - errmsg_written_bytes);
	    }
	  return;
	}
    }
  cas_coarray_alloc_work (desc, elem_size, corank, alloc_type);
  sync_all (&local->si);
}

void
cas_coarray_free (gfc_array_void *desc, int alloc_type)
{
  int i;
  if (alloc_type == GFC_NCA_LOCK_COARRAY)
    {
      lock_array *la;
      int expected = 0;
      la = desc->base_addr - offsetof (lock_array, arr);
      /* TODO: Fix this, replace with some kind of atomic initilization.  */
      while (!__atomic_compare_exchange_n (&la->owner, &expected,
					   this_image.image_num + 1, false,
					   __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
	;
      if (!--la->initialized)
	{
	  /* Coarray locks can be removed and just normal
	     pthread_mutex can be used.	 */
	  for (i = 0; i < local->total_num_images; i++)
	    pthread_mutex_destroy (&la->arr[i]);
	}
      __atomic_store_n (&la->owner, 0, __ATOMIC_SEQ_CST);
    }
  else if (alloc_type == GFC_NCA_EVENT_COARRAY)
    (void)0; // TODO

  free_memory_with_id (&local->ai, (intptr_t)desc);
  desc->base_addr = NULL;
}

int
cas_coarray_this_image (int distance __attribute__ ((unused)))
{
  return this_image.image_num + 1;
}

int
cas_coarray_num_images (int distance __attribute__ ((unused)))
{
  return local->total_num_images;
}

void
cas_coarray_sync_all (int *stat)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, NULL, 0);
  sync_all (&local->si);
}

void
cas_sync_images (int s, int *images, int *stat, char *error,
		 size_t err_size)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, error, err_size);
  sync_table (&local->si, images, s);
}

void
cas_lock (void *lock)
{
  pthread_mutex_lock (lock);
}

void
cas_unlock (void *lock)
{
  pthread_mutex_unlock (lock);
}

void
cas_collsub_reduce_array (gfc_array_char *desc,
			  void (*assign_function) (void *, void *),
			  int *result_image, int *stat, char *errmsg,
			  size_t errmsg_len)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, errmsg, errmsg_len);
  collsub_reduce_array (&local->ci, desc, result_image, assign_function);
}

void
cas_collsub_reduce_scalar (void *obj, index_type elem_size,
			   void (*assign_function) (void *, void *),
			   int *result_image, int *stat, char *errmsg,
			   size_t errmsg_len)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, errmsg, errmsg_len);
  collsub_reduce_scalar (&local->ci, obj, elem_size, result_image,
			 assign_function);
}

void
cas_collsub_broadcast_array (gfc_array_char *restrict a, int source_image,
			     int *stat, char *errmsg, size_t errmsg_len)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, errmsg, errmsg_len);
  collsub_broadcast_array (&local->ci, a, source_image - 1);
}

void
cas_collsub_broadcast_scalar (void *restrict obj, size_t size,
			      int source_image, int *stat, char *errmsg,
			      size_t errmsg_len)
{
  STAT_ERRMSG_ENTRY_CHECK (stat, errmsg, errmsg_len);
  collsub_broadcast_scalar (&local->ci, obj, size, source_image - 1);
}
