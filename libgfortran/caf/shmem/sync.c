/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
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
#include "supervisor.h"
#include "sync.h"
#include "teams_mgmt.h"
#include "thread_support.h"

#include <string.h>

static inline void
lock_table (sync_t *si)
{
  pthread_mutex_lock (&si->cis->sync_images_table_lock);
}

static inline void
unlock_table (sync_t *si)
{
  pthread_mutex_unlock (&si->cis->sync_images_table_lock);
}

void
sync_init (sync_t *si, shared_memory sm)
{
  *si = (sync_t) {
    &this_image.supervisor->sync_shared,
    SHMPTR_AS (int *, this_image.supervisor->sync_shared.sync_images_table, sm),
    SHMPTR_AS (pthread_cond_t *,
	       this_image.supervisor->sync_shared.sync_images_cond_vars, sm)};
}

void
sync_init_supervisor (sync_t *si, alloc *ai)
{
  const int num_images = local->total_num_images;
  const size_t table_size_in_bytes = sizeof (int) * num_images * num_images;

  si->cis = &this_image.supervisor->sync_shared;

  initialize_shared_mutex (&si->cis->event_lock);
  initialize_shared_condition (&si->cis->event_cond);

  initialize_shared_mutex (&si->cis->sync_images_table_lock);

  si->cis->sync_images_table
    = allocator_shared_malloc (alloc_get_allocator (ai), table_size_in_bytes);
  si->cis->sync_images_cond_vars
    = allocator_shared_malloc (alloc_get_allocator (ai),
			       sizeof (pthread_cond_t) * num_images);

  si->table = SHMPTR_AS (int *, si->cis->sync_images_table, ai->mem);
  si->triggers
    = SHMPTR_AS (pthread_cond_t *, si->cis->sync_images_cond_vars, ai->mem);

  for (int i = 0; i < num_images; i++)
    initialize_shared_condition (&si->triggers[i]);

  memset (si->table, 0, table_size_in_bytes);
}

void
sync_table (sync_t *si, int *images, int size)
{
  /* The variable `table` is an N x N matrix, where N is the number of all
     images.  The position (i, j) (where i and j are always the real images
     index, i.e. after team de-mapping) tells whether image i has seen the same
     number of synchronisation calls to sync_table like j.  When table(i,j) ==
     table(j,i) then the sync for i with this image is completed (here j is the
     real image index of the current image).  When this holds for all i in the
     current set of images (or all images, if the set is empty), then sync table
     command is completed.
     */
  volatile int *table = si->table;
  int i;

  lock_table (si);
  if (size > 0)
    {
      const size_t img_c = caf_current_team->u.image_info->image_map_size;
      for (i = 0; i < size; ++i)
	{
	  ++table[images[i] + img_c * this_image.image_num];
	  pthread_cond_signal (&si->triggers[images[i]]);
	}
      for (;;)
	{
	  for (i = 0; i < size; ++i)
	    if (this_image.supervisor->images[images[i]].status == IMAGE_OK
		&& table[images[i] + this_image.image_num * img_c]
		     > table[this_image.image_num + images[i] * img_c])
	      break;
	  if (i == size)
	    break;
	  pthread_cond_wait (&si->triggers[this_image.image_num],
			     &si->cis->sync_images_table_lock);
	}
    }
  else
    {
      int *map = caf_current_team->u.image_info->image_map;
      size = caf_current_team->u.image_info->image_count.count;
      for (i = 0; i < size; ++i)
	{
	  if (this_image.supervisor->images[map[i]].status != IMAGE_OK)
	    continue;
	  ++table[map[i] + size * this_image.image_num];
	  pthread_cond_signal (&si->triggers[map[i]]);
	}
      for (;;)
	{
	  for (i = 0; i < size; ++i)
	    if (this_image.supervisor->images[map[i]].status == IMAGE_OK
		&& table[map[i] + size * this_image.image_num]
		     > table[this_image.image_num + map[i] * size])
	      break;
	  if (i == size)
	    break;
	  pthread_cond_wait (&si->triggers[this_image.image_num],
			     &si->cis->sync_images_table_lock);
	}
    }
  unlock_table (si);
}

void
sync_all (void)
{
  counter_barrier_wait (&caf_current_team->u.image_info->image_count);
}

void
sync_team (caf_shmem_team_t team)
{
  counter_barrier_wait (&team->u.image_info->image_count);
}

void
lock_event (sync_t *si)
{
  pthread_mutex_lock (&si->cis->event_lock);
}

void
unlock_event (sync_t *si)
{
  pthread_mutex_unlock (&si->cis->event_lock);
}

void
event_post (sync_t *si)
{
  pthread_cond_broadcast (&si->cis->event_cond);
}

void
event_wait (sync_t *si)
{
  pthread_cond_wait (&si->cis->event_cond, &si->cis->event_lock);
}
