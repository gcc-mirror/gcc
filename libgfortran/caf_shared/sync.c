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

#include <string.h>

static void
sync_all_init (counter_barrier *b)
{
  master_bind_active_image_barrier (this_image.m, b);
}

static inline void
lock_table (sync_iface *si)
{
  pthread_mutex_lock (&si->cis->table_lock);
}

static inline void
unlock_table (sync_iface *si)
{
  pthread_mutex_unlock (&si->cis->table_lock);
}

static inline void
wait_table_cond (sync_iface *si, pthread_cond_t *cond)
{
  pthread_cond_wait (cond, &si->cis->table_lock);
}

static int *
get_locked_table (sync_iface *si)
{
  lock_table (si);
  return si->table;
}

void
sync_iface_init (sync_iface *si, alloc_iface *ai, shared_memory *sm)
{
  si->cis = SHMPTR_AS (
      sync_iface_shared *,
      shared_malloc (get_allocator (ai), sizeof (sync_iface_shared)), sm);

  sync_all_init (&si->cis->sync_all);
  initialize_shared_mutex (&si->cis->table_lock);
  si->sm = sm;
  si->a = get_allocator (ai);

  si->cis->table = shared_malloc (si->a, sizeof (int) * local->total_num_images
					     * local->total_num_images);
  si->cis->triggers = shared_malloc (si->a, sizeof (pthread_cond_t)
						* local->total_num_images);

  si->table = SHMPTR_AS (int *, si->cis->table, si->sm);
  si->triggers = SHMPTR_AS (pthread_cond_t *, si->cis->triggers, si->sm);

  for (int i = 0; i < local->total_num_images; i++)
    initialize_shared_condition (&si->triggers[i]);
}

/* TODO: Maybe check whether synchronizing image is still alive.  */
void
sync_table (sync_iface *si, int *images, int size)
{
#if defined(DEBUG_NATIVE_COARRAY) && DEBUG_NATIVE_COARRAY
  dprintf (2,
	   "Image %d waiting for these %ld images: ", this_image.image_num + 1,
	   size);
  for (int d_i = 0; (size_t)d_i < size; d_i++)
    dprintf (2, "%d ", images[d_i]);
  dprintf (2, "\n");
#endif
  int i;
  int done;
  int *table = get_locked_table (si);
  if (size > 0)
    {
      for (i = 0; i < size; i++)
	{
	  table[images[i] - 1 + local->total_num_images * this_image.image_num]++;
	  pthread_cond_signal (&si->triggers[images[i] - 1]);
	}
      for (;;)
	{
	  done = 1;
	  for (i = 0; i < size; i++)
	    done &= si->table[images[i] - 1
			      + this_image.image_num * local->total_num_images]
	      == si->table[this_image.image_num
			   + (images[i] - 1) * local->total_num_images];
	  if (done)
	    break;
	  wait_table_cond (si, &si->triggers[this_image.image_num]);
	}
    }
  else
    {
      size = local->total_num_images;
      for (i = 0; i < size; i++)
	{
	  table[i + local->total_num_images * this_image.image_num]++;
	  pthread_cond_signal (&si->triggers[i]);
	}
      for (;;)
	{
	  done = 1;
	  for (i = 0; i < size; i++)
	    done &= si->table[i + this_image.image_num * local->total_num_images]
	      == si->table[this_image.image_num
			   + i * local->total_num_images];
	  if (done)
	    break;
	  wait_table_cond (si, &si->triggers[this_image.image_num]);
	}
    }
  unlock_table (si);
}

void
sync_all (sync_iface *si)
{
  counter_barrier_wait (&si->cis->sync_all);
}
