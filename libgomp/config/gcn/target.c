/* Copyright (C) 2017-2021 Free Software Foundation, Inc.
   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libgomp.h"
#include <limits.h>

void
GOMP_teams (unsigned int num_teams, unsigned int thread_limit)
{
  if (thread_limit)
    {
      struct gomp_task_icv *icv = gomp_icv (true);
      icv->thread_limit_var
	= thread_limit > INT_MAX ? UINT_MAX : thread_limit;
    }
  unsigned int num_workgroups, workgroup_id;
  num_workgroups = __builtin_gcn_dim_size (0);
  workgroup_id = __builtin_gcn_dim_pos (0);
  if (!num_teams || num_teams >= num_workgroups)
    num_teams = num_workgroups;
  else if (workgroup_id >= num_teams)
    {
      gomp_free_thread (gcn_thrs ());
      exit (0);
    }
  gomp_num_teams_var = num_teams - 1;
}

int
omp_pause_resource (omp_pause_resource_t kind, int device_num)
{
  (void) kind;
  (void) device_num;
  return -1;
}

int
omp_pause_resource_all (omp_pause_resource_t kind)
{
  (void) kind;
  return -1;
}

ialias (omp_pause_resource)
ialias (omp_pause_resource_all)

void
GOMP_target_ext (int device, void (*fn) (void *), size_t mapnum,
		 void **hostaddrs, size_t *sizes, unsigned short *kinds,
		 unsigned int flags, void **depend, void **args)
{
  (void) device;
  (void) fn;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  (void) flags;
  (void) depend;
  (void) args;
  __builtin_unreachable ();
}

void
GOMP_target_data_ext (int device, size_t mapnum, void **hostaddrs,
		      size_t *sizes, unsigned short *kinds)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  __builtin_unreachable ();
}

void
GOMP_target_end_data (void)
{
  __builtin_unreachable ();
}

void
GOMP_target_update_ext (int device, size_t mapnum, void **hostaddrs,
			size_t *sizes, unsigned short *kinds,
			unsigned int flags, void **depend)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  (void) flags;
  (void) depend;
  __builtin_unreachable ();
}

void
GOMP_target_enter_exit_data (int device, size_t mapnum, void **hostaddrs,
			     size_t *sizes, unsigned short *kinds,
			     unsigned int flags, void **depend)
{
  (void) device;
  (void) mapnum;
  (void) hostaddrs;
  (void) sizes;
  (void) kinds;
  (void) flags;
  (void) depend;
  __builtin_unreachable ();
}
