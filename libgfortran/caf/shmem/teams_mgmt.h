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

#ifndef TEAMS_MGMT_H
#define TEAMS_MGMT_H

#include "alloc.h"
#include "collective_subroutine.h"
#include "supervisor.h"

struct caf_shmem_team
{
  struct caf_shmem_team *parent;
  int team_no;
  /* The index is the image's index minus one in this team.  I.e. if in Fortran
     notion the current image is 3, then the value of index is 2.  This allows
     access to the image_map without having to substract one each time (and
     missing it).  Returning the image's index to the user is rarer, so adding
     one there is cheaper.  */
  int index;
  /* The last memid the parent team used.  This is used to restore the memid
     on an end team.  */
  memid parent_teams_last_active_memid;
  struct coarray_allocated
  {
    struct coarray_allocated *next;
    caf_shmem_token_t token;
  } *allocated;
  union
  {
    void *shm;
    struct shmem_image_info
    {
      counter_barrier image_count;
      struct collsub_shared collsub;
      int team_parent_id;
      int team_id;
      int image_map_size;
      /* Store the last known number of terminated images (either stopped or
	 failed) images.  On each access where all images need to be present
	 this is checked against the global number and the image_count and
	 image_map is updated.  */
      int num_term_images;
      memid lastmemid;
      int image_map[];
    } *image_info;
  } u;
};
typedef struct caf_shmem_team *caf_shmem_team_t;

/* The team currently active.  */
extern caf_shmem_team_t caf_current_team;

/* The initial team.  */
extern caf_shmem_team_t caf_initial_team;

/* Teams formed, but not in used currently.  */
extern caf_shmem_team_t caf_teams_formed;

#define CHECK_TEAM_INTEGRITY(team)                                             \
  if (unlikely (team->u.image_info->num_term_images                            \
		!= this_image.supervisor->failed_images                        \
		     + this_image.supervisor->finished_images))                \
  update_teams_images (team)

void update_teams_images (caf_shmem_team_t);

void check_health (int *, char *, size_t);

#define HEALTH_CHECK(stat, errmsg, errlen) check_health (stat, errmsg, errlen)

#endif
