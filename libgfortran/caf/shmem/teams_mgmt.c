/* Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#include "teams_mgmt.h"
#include "../caf_error.h"

caf_shmem_team_t caf_current_team = NULL, caf_initial_team;
caf_shmem_team_t caf_teams_formed = NULL;

/* Count the images among the COUNT images in MAP that have status STATUS.  */

static int
count_images (const int *map, int count, image_status status)
{
  int i, n = 0;

  for (i = 0; i < count; ++i)
    if (this_image.supervisor->images[map[i]].status == status)
      ++n;

  return n;
}

/* Get the number of images of the team INFO belongs to that have
   terminated.  */

static int
team_terminated_images (struct shmem_image_info *info)
{
  const int sz = info->image_map_size;
  int i, term = 0;

  for (i = 0; i < sz; ++i)
    if (this_image.supervisor->images[info->image_map[i]].status != IMAGE_OK)
      ++term;

  return term;
}

static void
update_teams_images_locked (struct shmem_image_info *info)
{
  if (info->num_term_images != this_image.supervisor->finished_images
				 + this_image.supervisor->failed_images)
    {
      const int old_num = info->num_term_images;

      info->num_term_images = team_terminated_images (info);

      counter_barrier_add_locked (&info->image_count,
				  old_num - info->num_term_images);
    }
}

void
update_teams_images (caf_shmem_team_t team)
{
  caf_shmem_mutex_lock (&team->u.image_info->image_count.mutex);
  update_teams_images_locked (team->u.image_info);
  caf_shmem_mutex_unlock (&team->u.image_info->image_count.mutex);
}

/* Drop this image from the barriers of TEAM.  */

static void
leave_team (struct shmem_image_info *info, bool stopped)
{
  counter_barrier *b = &info->image_count;
  counter_barrier *cb = &info->collsub.barrier;

  caf_shmem_mutex_lock (&b->mutex);
  update_teams_images_locked (info);
  if (stopped)
    counter_barrier_abort_locked (b);
  caf_shmem_mutex_unlock (&b->mutex);

  caf_shmem_mutex_lock (&cb->mutex);
  counter_barrier_abort_locked (cb);
  caf_shmem_mutex_unlock (&cb->mutex);
}

void
leave_teams (bool stopped)
{
  for (caf_shmem_team_t t = caf_current_team; t; t = t->parent)
    leave_team (t->u.image_info, stopped);
  for (caf_shmem_team_t t = caf_teams_formed; t; t = t->parent)
    leave_team (t->u.image_info, stopped);
}

void
register_team (caf_shmem_team_t team)
{
  struct shmem_image_info *info = team->u.image_info;
  const shared_mem_ptr self = AS_SHMPTR ((void *) info, local->sm);
  ptrdiff_t head;

  /* Push without taking a lock, so that an image killed in the middle of it
     cannot block the supervisor.  */
  head = __atomic_load_n (&this_image.supervisor->teams.offset,
			  __ATOMIC_ACQUIRE);
  do
    info->next_team.offset = head;
  while (!__atomic_compare_exchange_n (&this_image.supervisor->teams.offset,
				       &head, self.offset, false,
				       __ATOMIC_RELEASE, __ATOMIC_ACQUIRE));
}

void
update_registered_teams (void)
{
  ptrdiff_t off = __atomic_load_n (&this_image.supervisor->teams.offset,
				   __ATOMIC_ACQUIRE);

  while (off != SHMPTR_NULL.offset)
    {
      struct shmem_image_info *info
	= SHMPTR_AS (struct shmem_image_info *, ((shared_mem_ptr) {off}),
		     &local->sm);

      /* A terminated image is dropped from the count, so that the others can
	 complete their barriers, but a collective over a fixed set of images
	 can no longer be completed.  */
      leave_team (info, false);
      off = __atomic_load_n (&info->next_team.offset, __ATOMIC_ACQUIRE);
    }
}

int
check_health (const int *map, int count, int *stat, char *errmsg,
	      size_t errmsg_len)
{
  int stopped = 0, failed = 0;

  if (this_image.supervisor->finished_images)
    stopped = count_images (map, count, IMAGE_SUCCESS);
  if (this_image.supervisor->failed_images)
    failed = count_images (map, count, IMAGE_FAILED);

  if (stopped)
    {
      caf_internal_error ("Stopped images present (currently %d)", stat,
			  errmsg, errmsg_len, stopped);
      if (stat)
	*stat = CAF_STAT_STOPPED_IMAGE;
      return CAF_STAT_STOPPED_IMAGE;
    }

  if (failed)
    {
      caf_internal_error ("Failed images present (currently %d)", stat,
			  errmsg, errmsg_len, failed);
      if (stat)
	*stat = CAF_STAT_FAILED_IMAGE;
      return CAF_STAT_FAILED_IMAGE;
    }

  if (stat)
    *stat = 0;
  return 0;
}
