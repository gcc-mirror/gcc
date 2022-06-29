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

/* This file contains the source code of functions
   declared in ompd-helper.h.  */

#include "ompd-helper.h"

ompd_device_type_sizes_t target_sizes;

/* Get global ICVs.  */
ompd_rc_t
gompd_get_cancellation (ompd_address_space_handle_t *ah,
			ompd_word_t *cancel_var)
{
  CHECK (ah);
  ompd_word_t cancel = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_cancel_var", cancel, cancel,
	     target_sizes.sizeof_char, 1, ret, symbol_addr);
  *cancel_var = cancel;
  return ret;
}

ompd_rc_t
gompd_get_max_task_priority (ompd_address_space_handle_t *ah,
			     ompd_word_t *task_p)
{
  CHECK (ah);
  ompd_word_t task_priority = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_max_task_priority_var", task_priority,
	     task_priority, target_sizes.sizeof_int, 1, ret, symbol_addr);
  *task_p = task_priority;
  return ret;
}

ompd_rc_t
gompd_get_stacksize (ompd_address_space_handle_t *ah, ompd_word_t *stacksize)
{
  CHECK (ah);
  ompd_word_t stack_var = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "stacksize", stack_var, stack_var,
	     target_sizes.sizeof_long, 1, ret, symbol_addr);
  *stacksize = stack_var;
  return ret;
}

ompd_rc_t
gompd_get_debug (ompd_address_space_handle_t *ah, ompd_word_t *debug_var)
{
  CHECK (ah);
  ompd_word_t debug = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_debug_var", debug, debug,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  *debug_var = debug;
  return ret;
}

ompd_rc_t
gompd_get_display_affinity (ompd_address_space_handle_t *ah, ompd_word_t *aff)
{
  CHECK (ah);
  ompd_word_t affin = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_display_affinity_var", affin, affin,
	     target_sizes.sizeof_char, 1, ret, symbol_addr);
  *aff = affin;
  return ret;
}

ompd_rc_t
gompd_get_affinity_format_len (ompd_address_space_handle_t *ah,
			       ompd_word_t *len)
{
  CHECK (ah);
  ompd_word_t len_var = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_affinity_format_len", len_var, len_var,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  *len = len_var;
  return ret;
}

ompd_rc_t
gompd_get_affinity_format (ompd_address_space_handle_t *ah, const char **string)
{
  CHECK (ah);
  ompd_word_t len = 100;
  ompd_rc_t ret;
  char *temp_str;
  ompd_word_t addr;
  ret = callbacks->alloc_memory (len + 1, (void **) &temp_str);
  CHECK_RET (ret);
  temp_str[len] = '\0';
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  ret = callbacks->symbol_addr_lookup (ah->context, NULL,
				       "gomp_affinity_format_var", &symbol_addr,
				       NULL);
  CHECK_RET (ret);
  ret = callbacks->read_memory (ah->context, NULL, &symbol_addr,
				target_sizes.sizeof_pointer, &addr);
  CHECK_RET (ret);
  symbol_addr.address = addr;
  ret = callbacks->read_string (ah->context, NULL, &symbol_addr, len,
				(void *) temp_str);
  CHECK_RET (ret);
  ret = callbacks->device_to_host (ah->context, &temp_str,
				   target_sizes.sizeof_char, len, &temp_str);
  *string = temp_str;
  return ret;
}

ompd_rc_t
gompd_get_wait_policy (ompd_address_space_handle_t *ah,
		       ompd_word_t *wait_policy)
{
  CHECK (ah);
  ompd_word_t wait_p = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "wait_policy", wait_p, wait_p,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  *wait_policy = wait_p;
  return ret;
}

ompd_rc_t
gompd_get_num_teams (ompd_address_space_handle_t *ah, ompd_word_t *num_teams)
{
  CHECK (ah);
  ompd_word_t num_t = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_num_teams_var", num_t, num_t,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  *num_teams = num_t;
  return ret;
}

ompd_rc_t
gompd_get_teams_thread_limit (ompd_address_space_handle_t *ah,
			      ompd_word_t *thread_limit)
{
  CHECK (ah);
  ompd_word_t thr_lim = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_teams_thread_limit_var", thr_lim, thr_lim,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  *thread_limit = thr_lim;
  return ret;
}

ompd_rc_t
gompd_get_spin_count (ompd_address_space_handle_t *ah, ompd_word_t *spin_count)
{
  CHECK (ah);
  ompd_word_t spins = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_spin_count_var", spins, spins,
	     target_sizes.sizeof_long_long, 1, ret, symbol_addr);
  *spin_count = spins;
  return ret;
}

ompd_rc_t
gompd_get_available_cpus (ompd_address_space_handle_t *ah, ompd_word_t *procs)
{
  CHECK (ah);
  ompd_word_t cpus = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_available_cpus", cpus, cpus,
	     target_sizes.sizeof_long, 1, ret, symbol_addr);
  *procs = cpus;
  return ret;
}

ompd_rc_t
gompd_get_throttled_spin_count (ompd_address_space_handle_t *ah,
				ompd_word_t *throt)
{
  CHECK (ah);
  ompd_word_t temp = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_throttled_spin_count_var", temp, temp,
	     target_sizes.sizeof_long_long, 1, ret, symbol_addr);
  *throt = temp;
  return ret;
}

ompd_rc_t
gompd_get_managed_threads (ompd_address_space_handle_t *ah, ompd_word_t *man_th)
{
  CHECK (ah);
  ompd_word_t temp = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gomp_managed_threads", temp, temp,
	     target_sizes.sizeof_long, 1, ret, symbol_addr);
  *man_th = temp;
  return ret;
}

ompd_rc_t
gompd_get_gompd_enabled (ompd_address_space_handle_t *ah, const char **string)
{
  CHECK (ah);
  ompd_word_t temp = 0;
  ompd_rc_t ret;
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (ah->context, NULL, "gompd_enabled", temp, temp,
	     target_sizes.sizeof_int, 1, ret, symbol_addr);
  static const char *temp_string = "disabled";
  if (temp == 1)
    temp_string = "enabled";
  else if (temp == -1)
    temp_string = "undefined";
  *string = temp_string;
  return ret;
}
/* End of global ICVs functions.  */

/* Get per thread ICVs.  */
ompd_rc_t
gompd_get_nthread (ompd_thread_handle_t *thread_handle,
		   ompd_word_t *nthreads_var)
{
  /* gomp_thread->task->gomp_task_icv.nthreads_var.  */
  if (thread_handle == NULL)
    return ompd_rc_stale_handle;
  if (nthreads_var == NULL)
    return ompd_rc_bad_input;
  CHECK (thread_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = thread_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = thread_handle->ah->context;
  ompd_thread_context_t *t_context = thread_handle->thread_context;
  ompd_rc_t ret;
  /* gomp_thread->task.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_thread_task",
		temp_offset, 1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.nthreads_var.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_task_icv_nthreads_var",
		temp_offset, 0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, t_context, symbol_addr, target_sizes.sizeof_long_long,
	       1, res, ret, 0);
  *nthreads_var = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_default_device (ompd_thread_handle_t *thread_handle,
			  ompd_word_t *default_device_var)
{
  /* gomp_thread->task->gomp_task_icv.default_device_var.  */
  if (thread_handle == NULL)
    return ompd_rc_stale_handle;
  if (default_device_var == NULL)
    return ompd_rc_bad_input;
  CHECK (thread_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = thread_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = thread_handle->ah->context;
  ompd_thread_context_t *t_context = thread_handle->thread_context;
  ompd_rc_t ret;
  /* gomp_thread->task.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_thread_task",
		temp_offset, 1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.default_device_var.  */
  ACCESS_VALUE (context, t_context,
		"gompd_access_gomp_task_icv_default_device_var", temp_offset, 0,
		ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, t_context, symbol_addr, target_sizes.sizeof_int, 1,
	       res, ret, 0);
  *default_device_var = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_dynamic (ompd_thread_handle_t *thread_handle, ompd_word_t *dyn_var)
{
  /* gomp_thread->task->gomp_task_icv.dyn_var.  */
  if (thread_handle == NULL)
    return ompd_rc_stale_handle;
  if (dyn_var == NULL)
    return ompd_rc_bad_input;
  CHECK (thread_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = thread_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = thread_handle->ah->context;
  ompd_thread_context_t *t_context = thread_handle->thread_context;
  ompd_rc_t ret;
  /* gomp_thread->task.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_thread_task",
		temp_offset, 1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_thread->task->task_icv.dyn_var.  */
  ACCESS_VALUE (context, t_context, "gompd_access_gomp_task_icv_dyn_var",
		temp_offset, 0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, t_context, symbol_addr, target_sizes.sizeof_char, 1,
	       res, ret, 0);
  *dyn_var = res;
  return ompd_rc_ok;
}
/* End of per thread ICVs.  */


/* Get per task ICVs.  */

ompd_rc_t
gompd_get_thread_limit (ompd_task_handle_t *task_handle,
			ompd_word_t *thread_limit_var)
{
  /* gomp_task->gomp_task_icv.thread_limit_var.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (thread_limit_var == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->task_icv.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_task->task_icv.thread_limit_var.  */
  ACCESS_VALUE (context, NULL,
		"gompd_access_gomp_task_icv_thread_limit_var", temp_offset, 0,
		ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_int, 1,
	       res, ret, 0);
  *thread_limit_var = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_run_sched_chunk_size (ompd_task_handle_t *task_handle,
				ompd_word_t *run_sched_chunk_size)
{
  /* gomp_task->gomp_task_icv.run_sched_chunk_size.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (run_sched_chunk_size == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->task_icv.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_task->task_icv.run_sched_chunk_size.  */
  ACCESS_VALUE (context, NULL,
		"gompd_access_gomp_task_icv_run_sched_chunk_size", temp_offset,
		0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_int, 1,
	       res, ret, 0);
  *run_sched_chunk_size = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_run_sched (ompd_task_handle_t *task_handle,
		     ompd_word_t *run_sched_var)
{
  /* gomp_task->gomp_task_icv.run_sched_var.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (run_sched_var == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->task_icv.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_task->task_icv.run_sched_var.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv_run_sched_var",
		temp_offset, 0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_int, 1,
	       res, ret, 0);
  *run_sched_var = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_max_active_levels (ompd_task_handle_t *task_handle,
			     ompd_word_t *max_active_levels_var)
{
  /* gomp_task->gomp_task_icv.max_active_levels_var.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (max_active_levels_var == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->task_icv.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_task->task_icv.run_sched_var.  */
  ACCESS_VALUE (context, NULL,
		"gompd_access_gomp_task_icv_max_active_levels_var", temp_offset,
		0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_char, 1,
	       res, ret, 0);
  *max_active_levels_var = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_get_proc_bind (ompd_task_handle_t *task_handle, const char **bind_var)
{
  /* gomp_task->gomp_task_icv.bind_var.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (bind_var == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->task_icv.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv", temp_offset,
		1, ret, symbol_addr, temp_sym_addr, temp_addr);
  /* gomp_task->task_icv.bind_var.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_icv_bind_var",
		temp_offset, 0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_char, 1,
	       res, ret, 0);
  static const char *temp_string = "undefined";
  if (res == 0)
    temp_string = "FALSE";
  else if (res == 1)
    temp_string = "TRUE";
  else if (res == 2)
    temp_string = "PRIMARY";
  else if (res == 3)
    temp_string = "CLOSE";
  else if (res == 4)
    temp_string = "SPREAD";
  else
    return ompd_rc_error;
  *bind_var = temp_string;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_is_final (ompd_task_handle_t *task_handle, ompd_word_t *final_task)
{
  /* gomp_task->final_task.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (final_task == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = 0;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->final_task.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_final_task", temp_offset,
		0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_char, 1,
	       res, ret, 0);
  *final_task = res;
  return ompd_rc_ok;
}

ompd_rc_t
gompd_is_implicit (ompd_task_handle_t *task_handle, ompd_word_t *task_kind)
{
  /* gomp_task->kind.  */
  if (task_handle == NULL)
    return ompd_rc_stale_handle;
  if (task_kind == NULL)
    return ompd_rc_bad_input;
  CHECK (task_handle->ah);

  ompd_word_t res = -1;
  ompd_address_t symbol_addr = task_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = task_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_task->kind.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_task_kind", temp_offset, 0,
		ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_int, 1, res,
	       ret, 0);
  /* if task is implicit, then res = 0.  */
  res = res ? -1: 0;
  *task_kind = res;
  return ompd_rc_ok;
}
/* End of task ICVs.  */

/* Get per parallel handle ICVs.  */
ompd_rc_t
gompd_get_team_size (ompd_parallel_handle_t *parallel_handle,
		     ompd_word_t *nthreads)
{
  /* gomp_team->nthreads.  */
  if (parallel_handle == NULL)
    return ompd_rc_stale_handle;
  if (nthreads == NULL)
    return ompd_rc_bad_input;
  CHECK (parallel_handle->ah);

  ompd_word_t res = -1;
  ompd_address_t symbol_addr = parallel_handle->th;
  ompd_word_t temp_offset;
  ompd_address_t temp_sym_addr;
  ompd_addr_t temp_addr;
  ompd_address_space_context_t *context = parallel_handle->ah->context;
  ompd_rc_t ret;
  /* gomp_team->nthreads.  */
  ACCESS_VALUE (context, NULL, "gompd_access_gomp_team_nthreads", temp_offset,
		0, ret, symbol_addr, temp_sym_addr, temp_addr);
  DEREFERENCE (context, NULL, symbol_addr, target_sizes.sizeof_int, 1,
	       res, ret, 0);
  *nthreads = res;
  return ompd_rc_ok;
}
/* End of parallel handle ICVs.  */

ompd_rc_t
gompd_get_sizes (ompd_address_space_context_t *context)
{
  if (context == NULL)
    return ompd_rc_bad_input;

  static bool inited = false;
  static ompd_rc_t ret;

  if (inited)
    return ret;

  ret = callbacks->sizeof_type (context, &target_sizes);
  if (ret != ompd_rc_ok)
    return ret;

  inited = true;
  return ret;
}
