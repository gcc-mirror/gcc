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

#include "libgfortran.h"
#include <string.h>
#include "libcoarraynative.h"
#include "collective_subroutine.h"
#include "allocator.h"
#include "counter_barrier.h"

#include <string.h>

void *
get_collsub_buf (collsub_iface *ci, size_t size)
{
  void *ret;

  pthread_mutex_lock (&ci->s->mutex);
  /* curr_size is always at least sizeof(double), so we don't need to worry
     about size == 0.  */
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

/* This function syncs all images with one another.  It will only return once
   all images have called it.  */

void
collsub_sync (collsub_iface *ci)
{
  counter_barrier_wait (&ci->s->barrier);
}

/* assign_function is needed since we only know how to assign the type inside
   the compiler.  It should be implemented as follows:

     void assign_function (void *a, void *b)
     {
       *((t *) a) = reduction_operation ((t *) a, (t *) b);
     }

   */

void
collsub_reduce_array (collsub_iface *ci, gfc_array_char *desc,
		      int *result_image,
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

  error_on_missing_images ();

  packed = pack_array_prepare (&pi, desc);
  if (pi.num_elem == 0)
    return;

  elem_size = GFC_DESCRIPTOR_SIZE (desc);
  this_image_size_bytes = elem_size * pi.num_elem;

  buffer
      = get_collsub_buf (ci, this_image_size_bytes * local->total_num_images);
  this_image_buf = buffer + this_image_size_bytes * this_image.image_num;

  if (packed)
    memcpy (this_image_buf, GFC_DESCRIPTOR_DATA (desc), this_image_size_bytes);
  else
    pack_array_finish (&pi, desc, this_image_buf);

  collsub_sync (ci);

  for (; ((this_image.image_num >> cbit) & 1) == 0
	 && (local->total_num_images >> cbit) != 0;
       cbit++)
    {
      imoffset = 1 << cbit;
      if (this_image.image_num + imoffset < local->total_num_images)
	/* Reduce arrays elementwise.  */
	for (ssize_t i = 0; i < pi.num_elem; i++)
	  assign_function (this_image_buf + elem_size * i,
			   this_image_buf + this_image_size_bytes * imoffset
			       + elem_size * i);

      collsub_sync (ci);
    }
  for (; (local->total_num_images >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || (*result_image - 1 ) == this_image.image_num)
    {
      if (packed)
	memcpy (GFC_DESCRIPTOR_DATA (desc), buffer, this_image_size_bytes);
      else
	unpack_array_finish (&pi, desc, buffer);
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

  error_on_missing_images ();

  buffer = get_collsub_buf (
      ci, elem_size * master_get_num_active_images (this_image.m));
  this_image_buf = buffer + elem_size * this_image.image_num;

  memcpy (this_image_buf, obj, elem_size);

  collsub_sync (ci);
  for (; ((this_image.image_num >> cbit) & 1) == 0
	 && (local->total_num_images >> cbit) != 0;
       cbit++)
    {
      imoffset = 1 << cbit;

      if (this_image.image_num + imoffset < local->total_num_images)
	{
	  /* Reduce arrays elementwise.  */
	  assign_function (this_image_buf,
			   this_image_buf + elem_size * imoffset);
	}
      collsub_sync (ci);
    }
  for (; (master_get_num_active_images (this_image.m) >> cbit) != 0; cbit++)
    collsub_sync (ci);

  if (!result_image || *result_image == this_image.image_num)
    memcpy (obj, buffer, elem_size);

  finish_collective_subroutine (ci);
}

/* Do not use sync_all(), because the program should deadlock in the case that
 * some images are on a sync_all barrier while others are in a collective
 * subroutine.  */

void
collsub_iface_init (collsub_iface *ci, alloc_iface *ai, shared_memory *sm)
{
  ci->s = SHARED_MEMORY_RAW_ALLOC_PTR (sm, collsub_iface_shared);

  ci->s->collsub_buf = shared_malloc (
      get_allocator (ai), sizeof (double) * local->total_num_images);
  ci->s->curr_size = sizeof (double) * local->total_num_images;
  ci->sm = sm;
  ci->a = get_allocator (ai);

  master_bind_active_image_barrier (this_image.m, &ci->s->barrier);
  initialize_shared_mutex (&ci->s->mutex);
}

void
collsub_broadcast_scalar (collsub_iface *ci, void *obj, index_type elem_size,
			  int source_image /* Adjusted in the wrapper.  */)
{
  void *buffer;

  buffer = get_collsub_buf (ci, elem_size);

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
	unpack_array_finish (&pi, desc, buffer);
    }

  finish_collective_subroutine (ci);
}
