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

#include "teams_mgmt.h"
#include "../caf_error.h"

caf_shmem_team_t caf_current_team = NULL, caf_initial_team;
caf_shmem_team_t caf_teams_formed = NULL;

void
update_teams_images (caf_shmem_team_t team)
{
  pthread_mutex_lock (&team->u.image_info->image_count.mutex);
  if (team->u.image_info->num_term_images
      != this_image.supervisor->finished_images
	   + this_image.supervisor->failed_images)
    {
      const int old_num = team->u.image_info->num_term_images;
      const int sz = team->u.image_info->image_map_size;
      int i, good = 0;

      for (i = 0; i < sz; ++i)
	if (this_image.supervisor->images[team->u.image_info->image_map[i]]
	      .status
	    == IMAGE_OK)
	  ++good;

      team->u.image_info->num_term_images = sz - good;

      counter_barrier_add_locked (&team->u.image_info->image_count,
				   old_num
				     - team->u.image_info->num_term_images);
    }
  pthread_mutex_unlock (&team->u.image_info->image_count.mutex);
}

void
check_health (int *stat, char *errmsg, size_t errmsg_len)
{
  if (this_image.supervisor->finished_images
      || this_image.supervisor->failed_images)
    {
      if (this_image.supervisor->finished_images)
	{
	  caf_internal_error ("Stopped images present (currently %d)", stat,
			      errmsg, errmsg_len,
			      this_image.supervisor->finished_images);
	  if (stat)
	    *stat = CAF_STAT_STOPPED_IMAGE;
	}
      else if (this_image.supervisor->failed_images)
	{
	  caf_internal_error ("Failed images present (currently %d)", stat,
			      errmsg, errmsg_len,
			      this_image.supervisor->failed_images);
	  if (stat)
	    *stat = CAF_STAT_FAILED_IMAGE;
	}
    }
  else if (stat)
    *stat = 0;
}
