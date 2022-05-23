/* Copyright (C) The GNU Toolchain Authors.
   Contributed by Mohamed Atef <mohamedatef1698@gmail.com>.
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

/* This file contains implementation of functions defined in 5.5.8 and 5.5.9
  in OpenMP Application Programming Interface v5.1.  */

#include "ompd-helper.h"

static const char *gompd_icv_string[] = { "undefined",
  #define gompd_icv_iterator(var_name, string_name, scope) string_name,
    FOREACH_OMPD_ICV (gompd_icv_iterator)
  #undef gompd_icv_iterator
};

static const ompd_scope_t gompd_icv_scope[] = {ompd_scope_global,
  #define gompd_icv_iterator(var_name, string_name, scope) scope,
    FOREACH_OMPD_ICV (gompd_icv_iterator)
  #undef gompd_icv_iterator
};

ompd_rc_t
ompd_enumerate_icvs (ompd_address_space_handle_t *ah,
		     ompd_icv_id_t current, ompd_icv_id_t *next_id,
		     const char **next_icv_name,
		     ompd_scope_t *next_scope, int *more)
{
  if (ah == NULL)
    return ompd_rc_stale_handle;
  if (current + 1 >= gompd_last_icv_var
      || next_id == NULL
      || next_icv_name == NULL
      || next_scope == NULL
      || more == NULL)
	return ompd_rc_bad_input;
  if (callbacks == NULL)
    return ompd_rc_callback_error;
  *next_id = current + 1;
  char *temp_name = NULL;
  ompd_rc_t ret
    = callbacks->alloc_memory (strlen (gompd_icv_string[*next_id]) + 1,
			       (void **) &temp_name);
  CHECK_RET (ret);
  strcpy (temp_name, gompd_icv_string[*next_id]);
  *next_icv_name = temp_name;
  *next_scope = gompd_icv_scope[*next_id];
  if ((*next_id) + 1 < gompd_last_icv_var)
    *more = 1;
  else
    *more = 0;
  return ompd_rc_ok;
}

ompd_rc_t
ompd_get_icv_from_scope (void *handle, ompd_scope_t scope, ompd_icv_id_t icv_id,
			 ompd_word_t *icv_value)
{
  if (handle == NULL)
    return ompd_rc_stale_handle;
  if (icv_value == NULL || !icv_id || icv_id >= gompd_last_icv_var)
    return ompd_rc_bad_input;
  if (callbacks == NULL)
    return ompd_rc_callback_error;
  ompd_device_t device;
  switch (scope)
    {
      case ompd_scope_address_space:
	device = ((ompd_address_space_handle_t *) handle)->kind;
	break;
      case ompd_scope_thread:
	device = ((ompd_thread_handle_t *) handle)->ah->kind;
	break;
      case ompd_scope_parallel:
	device = ((ompd_parallel_handle_t *) handle)->ah->kind;
	break;
      case ompd_scope_task:
	device = ((ompd_task_handle_t *) handle)->ah->kind;
	break;
      default:
	return ompd_rc_bad_input;
    }
  /* No offloading support for now.  */
  ompd_address_space_handle_t *ashandle
    = (ompd_address_space_handle_t *)handle;
  if (device == OMPD_DEVICE_KIND_HOST)
    {
      switch (icv_id)
	{
	  case gompd_icv_cancellation_var:
	    return gompd_get_cancellation (ashandle, icv_value);
	  case gompd_icv_max_task_priority_var:
	    return gompd_get_max_task_priority (ashandle, icv_value);
	  case gompd_icv_stacksize_var:
	    return gompd_get_stacksize (ashandle, icv_value);
	  case gompd_icv_debug_var:
	    return gompd_get_debug (ashandle, icv_value);
	  case gompd_icv_display_affinity_var:
	    return gompd_get_display_affinity (ashandle, icv_value);
	  case gompd_icv_affinity_format_var:
	    return ompd_rc_incompatible;
	  case gompd_icv_affinity_format_len_var:
	    return gompd_get_affinity_format_len (ashandle, icv_value);
	  case gompd_icv_wait_policy_var:
	    return gompd_get_wait_policy (ashandle, icv_value);
	  case gompd_icv_num_teams_var:
	    return gompd_get_num_teams (ashandle, icv_value);
	  case gompd_icv_teams_thread_limit_var:
	    return gompd_get_teams_thread_limit (ashandle, icv_value);
	  case gompd_icv_spin_count_var:
	    return gompd_get_spin_count (ashandle, icv_value);
	  case gompd_icv_num_proc_var:
	    return gompd_get_available_cpus (ashandle, icv_value);
	  case gompd_icv_throttled_spin_count_var:
	    return gompd_get_throttled_spin_count (ashandle, icv_value);
	  case gompd_icv_managed_threads_var:
	    return gompd_get_managed_threads (ashandle, icv_value);
	  default:
	    return ompd_rc_unsupported;
	}
    }
    return ompd_rc_error;
}

ompd_rc_t
ompd_get_icv_string_from_scope (void *handle, ompd_scope_t scope,
				ompd_icv_id_t icv_id, const char **icv_value)
{
  if (handle == NULL)
    return ompd_rc_stale_handle;
  if (icv_value == NULL || !icv_id || icv_id >= gompd_last_icv_var)
    return ompd_rc_bad_input;
  if (callbacks == NULL)
    return ompd_rc_callback_error;
  ompd_device_t device;
  switch (scope)
    {
      case ompd_scope_address_space:
	device = ((ompd_address_space_handle_t *) handle)->kind;
	break;
      case ompd_scope_thread:
	device = ((ompd_thread_handle_t *) handle)->ah->kind;
	break;
      case ompd_scope_parallel:
	device = ((ompd_parallel_handle_t *) handle)->ah->kind;
	break;
      case ompd_scope_task:
	device = ((ompd_task_handle_t *) handle)->ah->kind;
	break;
      default:
	return ompd_rc_bad_input;
    }
  /* No offloading support for now.  */
  ompd_address_space_handle_t *ashandle
    = (ompd_address_space_handle_t *)handle;
  if (device == OMPD_DEVICE_KIND_HOST)
    {
      switch (icv_id)
	{
	  case gompd_icv_affinity_format_var:
	    return gompd_get_affinity_format (ashandle, icv_value);
	  case gompd_icv_ompd_state:
	    return gompd_stringize_gompd_enabled (ashandle, icv_value);
	  default:
	    return ompd_rc_unsupported;
	}
    }
  return ompd_rc_error;
}
