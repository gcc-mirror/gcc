/* Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
#include "libgomp-gcn.h"
#include <limits.h>

extern volatile struct gomp_offload_icvs GOMP_ADDITIONAL_ICVS;

bool
GOMP_teams4 (unsigned int num_teams_lower, unsigned int num_teams_upper,
	     unsigned int thread_limit, bool first)
{
  if (!first)
    return false;
  if (thread_limit)
    {
      struct gomp_task_icv *icv = gomp_icv (true);
      icv->thread_limit_var
	= thread_limit > INT_MAX ? UINT_MAX : thread_limit;
    }
  unsigned int num_workgroups, workgroup_id;
  num_workgroups = __builtin_gcn_dim_size (0);
  workgroup_id = __builtin_gcn_dim_pos (0);
  /* FIXME: If num_teams_lower > num_workgroups, we want to loop
     multiple times at least for some workgroups.  */
  (void) num_teams_lower;
  if (!num_teams_upper || num_teams_upper >= num_workgroups)
    num_teams_upper = ((GOMP_ADDITIONAL_ICVS.nteams > 0
			&& num_workgroups > GOMP_ADDITIONAL_ICVS.nteams)
		       ? GOMP_ADDITIONAL_ICVS.nteams : num_workgroups);
  else if (workgroup_id >= num_teams_upper)
    return false;
  gomp_num_teams_var = num_teams_upper - 1;
  return true;
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
  (void) flags;
  (void) depend;
  (void) args;

  if (device != GOMP_DEVICE_HOST_FALLBACK || fn == NULL)
    return;

  /* The output data is at ((void*) kernargs)[2].  */
  register void **kernargs = (void**) __builtin_gcn_kernarg_ptr ();
  struct output *data = (struct output *) kernargs[2];
  /* Reserve one slot. */
  unsigned int index = __atomic_fetch_add (&data->next_output, 1,
					   __ATOMIC_ACQUIRE);

  if ((unsigned int) (index + 1) < data->consumed)
    abort ();  /* Overflow.  */

  /* Spinlock while the host catches up.  */
  if (index >= 1024)
    while (__atomic_load_n (&data->consumed, __ATOMIC_ACQUIRE)
	   <= (index - 1024))
      asm ("s_sleep 64");

  unsigned int slot = index % 1024;
  data->queue[slot].value_u64[0] = (uint64_t) fn;
  data->queue[slot].value_u64[1] = (uint64_t) mapnum;
  data->queue[slot].value_u64[2] = (uint64_t) hostaddrs;
  data->queue[slot].value_u64[3] = (uint64_t) sizes;
  data->queue[slot].value_u64[4] = (uint64_t) kinds;
  data->queue[slot].value_u64[5] = (uint64_t) GOMP_ADDITIONAL_ICVS.device_num;

  data->queue[slot].type = 4; /* Reverse offload.  */
  __atomic_store_n (&data->queue[slot].written, 1, __ATOMIC_RELEASE);

  /* Spinlock while the host catches up.  */
  while (__atomic_load_n (&data->queue[slot].written, __ATOMIC_ACQUIRE) != 0)
    asm ("s_sleep 64");
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
