/* Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

/* Implement OpenMP 'teams' construct.

   Initialize upon FIRST call.  Return whether this invocation is active.
   Depending on whether NUM_TEAMS_LOWER asks for more teams than are provided
   in hardware, we may need to loop multiple times; in that case make sure to
   update the team-level variable used by 'omp_get_team_num', as we then can't
   just use '__builtin_gcn_dim_pos (0)'.  */

bool
GOMP_teams4 (unsigned int num_teams_lower, unsigned int num_teams_upper,
	     unsigned int thread_limit, bool first)
{
  int __lds *gomp_team_num = (int __lds *) GOMP_TEAM_NUM;
  unsigned int num_workgroups = __builtin_gcn_dim_size (0);
  if (!first)
    {
      unsigned int team_num;
      if (num_workgroups > gomp_num_teams_var)
	return false;
      team_num = *gomp_team_num;
      if (team_num > gomp_num_teams_var - num_workgroups)
	return false;
      *gomp_team_num = team_num + num_workgroups;
      return true;
    }
  if (thread_limit)
    {
      struct gomp_task_icv *icv = gomp_icv (true);
      icv->thread_limit_var
	= thread_limit > INT_MAX ? UINT_MAX : thread_limit;
    }
  if (!num_teams_upper)
    num_teams_upper = ((GOMP_ADDITIONAL_ICVS.nteams > 0
			&& num_workgroups > GOMP_ADDITIONAL_ICVS.nteams)
		       ? GOMP_ADDITIONAL_ICVS.nteams : num_workgroups);
  else if (num_workgroups < num_teams_lower)
    num_teams_upper = num_teams_lower;
  else if (num_workgroups < num_teams_upper)
    num_teams_upper = num_workgroups;
  unsigned int workgroup_id = __builtin_gcn_dim_pos (0);
  if (workgroup_id >= num_teams_upper)
    return false;
  *gomp_team_num = workgroup_id;
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

int
omp_get_num_interop_properties (const omp_interop_t interop
				__attribute__ ((unused)))
{
  return 0;
}

omp_intptr_t
omp_get_interop_int (const omp_interop_t interop,
		     omp_interop_property_t property_id,
		     omp_interop_rc_t *ret_code)
{
  if (ret_code == NULL)
    return 0;
  if (property_id < omp_ipr_first || property_id >= 0)
    *ret_code = omp_irc_out_of_range;
  else if (interop == omp_interop_none)
    *ret_code = omp_irc_empty;
  else
    *ret_code = omp_irc_other;
  return 0;
}

void *
omp_get_interop_ptr (const omp_interop_t interop,
		     omp_interop_property_t property_id,
		     omp_interop_rc_t *ret_code)
{
  if (ret_code == NULL)
    return NULL;
  if (property_id < omp_ipr_first || property_id >= 0)
    *ret_code = omp_irc_out_of_range;
  else if (interop == omp_interop_none)
    *ret_code = omp_irc_empty;
  else
    *ret_code = omp_irc_other;
  return NULL;
}

const char *
omp_get_interop_str (const omp_interop_t interop,
		     omp_interop_property_t property_id,
		     omp_interop_rc_t *ret_code)
{
  if (ret_code == NULL)
    return NULL;
  if (property_id < omp_ipr_first || property_id >= 0)
    *ret_code = omp_irc_out_of_range;
  else if (interop == omp_interop_none)
    *ret_code = omp_irc_empty;
  else
    *ret_code = omp_irc_other;
  return NULL;
}

const char *
omp_get_interop_name (const omp_interop_t interop __attribute__ ((unused)),
		      omp_interop_property_t property_id)
{
  static const char *prop_string[0 - omp_ipr_first]
    = {"fr_id", "fr_name", "vendor", "vendor_name", "device_num", "platform",
       "device", "device_context", "targetsync"};
  if (property_id < omp_ipr_first || property_id >= 0)
    return NULL;
  return prop_string[omp_ipr_fr_id - property_id];
}

const char *
omp_get_interop_type_desc (const omp_interop_t interop __attribute__ ((unused)),
			   omp_interop_property_t property_id
			   __attribute__ ((unused)))
{
  return NULL;
}

const char *
omp_get_interop_rc_desc (const omp_interop_t interop __attribute__ ((unused)),
			 omp_interop_rc_t ret_code)
{
  static const char *rc_strings[omp_irc_no_value - omp_irc_other + 1]
    = {"no meaningful value available",
       "successful",
       "provided interoperability object is equal to omp_interop_none",
       "property ID is out of range",
       "property type is integer; use omp_get_interop_int",
       "property type is pointer; use omp_get_interop_ptr",
       "property type is string; use omp_get_interop_str",
       "obtaining properties is only supported on the initial device"};
  /* omp_irc_other is returned by device-side omp_get_interop_{int,ptr,str};
     the host returns for omp_irc_other NULL as it is  not used. Besides the
     three omp_interop_rc_t values used on the device side, handle host values
     leaked to the device side.  */
  if (ret_code > omp_irc_no_value || ret_code < omp_irc_other)
    return NULL;
  return rc_strings[omp_irc_no_value - ret_code];
}

const char *
omp_get_uid_from_device (int device_num __attribute__ ((unused)))
{
  return NULL;
}

int
omp_get_device_from_uid (const char *uid __attribute__ ((unused)))
{
  return omp_invalid_device;
}

ialias (omp_get_num_interop_properties)
ialias (omp_get_interop_int)
ialias (omp_get_interop_ptr)
ialias (omp_get_interop_str)
ialias (omp_get_interop_name)
ialias (omp_get_interop_type_desc)
ialias (omp_get_interop_rc_desc)
ialias (omp_get_uid_from_device)
ialias (omp_get_device_from_uid)
