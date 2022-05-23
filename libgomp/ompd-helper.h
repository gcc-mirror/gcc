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

/* This file contains data types and declarations of functions that are not
   provided by the book but we find them necessary.  */

#ifndef _OMPD_HELPER_H
#define _OMPD_HELPER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "omp-tools.h"
#include "ompd-types.h"
#include "config.h"
#include <assert.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define stringize(x) stringize1(x)
#define stringize1(x) #x

#define OMPD_VERSION 202011

extern const ompd_callbacks_t *callbacks;
extern __UINT64_TYPE__ gompd_state;
extern ompd_device_type_sizes_t target_sizes;

typedef struct _ompd_aspace_handle
{
  ompd_address_space_context_t *context;
  ompd_device_t kind;
} ompd_address_space_handle_t;

typedef struct _ompd_thread_handle
{
  ompd_address_space_handle_t *ah;
  ompd_thread_context_t *thread_context;
  ompd_address_t th;
} ompd_thread_handle_t;

typedef struct _ompd_parallel_handle
{
  ompd_address_space_handle_t *ah;
  ompd_address_t th;
} ompd_parallel_handle_t;

typedef struct _ompd_task_handle
{
  ompd_address_space_handle_t *ah;
  ompd_address_t th;
} ompd_task_handle_t;

#define CHECK_RET(ret) \
 do { \
   if (ret != ompd_rc_ok) \
     return ret; \
 } while (0)

#define GET_VALUE(context, thread_context, name, output, dist_buf, size, \
  count, ret, symbol_addr) \
  do { \
    ret = callbacks->symbol_addr_lookup (context, thread_context, name, \
					 &symbol_addr, NULL); \
    CHECK_RET (ret); \
    ret = callbacks->read_memory (context, thread_context, &symbol_addr, size, \
				  &dist_buf); \
    CHECK_RET (ret); \
    ret = callbacks->device_to_host (context, &dist_buf, size, count, &output);\
    CHECK_RET (ret); \
  } while (0)

#define CHECK(ah) \
  do {   \
    if (ah == NULL || ah->context == NULL) \
      return ompd_rc_stale_handle; \
    if (callbacks == NULL) \
      return ompd_rc_callback_error; \
  } while (0)

/* (var_name, string_name, scope).  */
#define FOREACH_OMPD_ICV(ompd_icv) \
  ompd_icv (nthreads_var, "nthread var", ompd_scope_thread) \
  ompd_icv (thread_limit_var, "thread limit var", ompd_scope_task) \
  ompd_icv (run_sched_var, "run sched limit var", ompd_scope_task) \
  ompd_icv (run_sched_chunk_size, "run sched chunk size var", ompd_scope_task) \
  ompd_icv (default_device_var, "default device var", ompd_scope_thread) \
  ompd_icv (dyn_var, "dynamic var", ompd_scope_thread) \
  ompd_icv (max_active_levels_var, "max active level var", ompd_scope_task) \
  ompd_icv (bind_var, "proc bind var", ompd_scope_task) \
  ompd_icv (cancellation_var, "cancel var", ompd_scope_address_space) \
  ompd_icv (max_task_priority_var, "max task priority var", \
	    ompd_scope_address_space) \
  ompd_icv (stacksize_var, "stack size var", ompd_scope_address_space) \
  ompd_icv (debug_var, "debug var", ompd_scope_address_space) \
  ompd_icv (ompd_state, "OMP_DEBUG", ompd_scope_address_space) \
  ompd_icv (display_affinity_var, "display affinity var", \
	    ompd_scope_address_space) \
  ompd_icv (affinity_format_var, "affinity format var", \
	    ompd_scope_address_space) \
  ompd_icv (affinity_format_len_var, "affinity format len var", \
	    ompd_scope_address_space) \
  ompd_icv (wait_policy_var, "wait policy var", ompd_scope_address_space) \
  ompd_icv (num_teams_var, "num teams var", ompd_scope_address_space) \
  ompd_icv (teams_thread_limit_var, "teams thread limit var", \
	    ompd_scope_address_space) \
  ompd_icv (spin_count_var, "spin count var", ompd_scope_address_space) \
  ompd_icv (num_proc_var, "num proc var", ompd_scope_address_space) \
  ompd_icv (throttled_spin_count_var, "throttled spin count var", \
	    ompd_scope_address_space) \
  ompd_icv (managed_threads_var, "managed threads var", \
	    ompd_scope_address_space) \
  ompd_icv (thread_num_var, "thread num var", ompd_scope_thread) \
  ompd_icv (final_task_var, "final task var", ompd_scope_task) \
  ompd_icv (implicit_task_var, "implicit task var", ompd_scope_task) \
  ompd_icv (team_size_var, "team size var", ompd_scope_parallel)

enum ompd_icv
{
  gompd_icv_undefined_var = 0,
  #define gompd_icv_iterator(var_name, string_name, scope) gompd_icv_##var_name,
    FOREACH_OMPD_ICV (gompd_icv_iterator)
  #undef gompd_icv_iterator
  gompd_last_icv_var
};

#ifdef HAVE_ATTRIBUTE_VISIBILITY
#pragma GCC visibility push(hidden)
#endif

ompd_rc_t gompd_get_sizes (ompd_address_space_context_t *);

/* Get Local internal control variables.  */
ompd_rc_t gompd_get_nthread (ompd_thread_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_thread_limit (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_run_sched (ompd_task_handle_t *,  ompd_word_t *);
ompd_rc_t gompd_get_run_sched_chunk_size (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_default_device (ompd_thread_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_dynamic (ompd_thread_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_max_active_levels (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_proc_bind (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_is_final (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_is_implicit (ompd_task_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_team_size (ompd_parallel_handle_t *, ompd_word_t *);

/* Get Global ICVs.  */
ompd_rc_t gompd_get_cancellation (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_max_task_priority (ompd_address_space_handle_t *,
  				       ompd_word_t *);
ompd_rc_t gompd_get_stacksize (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_debug (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_display_affinity (ompd_address_space_handle_t *,
				      ompd_word_t *);
ompd_rc_t gompd_get_affinity_format (ompd_address_space_handle_t *,
				     const char **);
ompd_rc_t gompd_get_affinity_format_len (ompd_address_space_handle_t *,
					 ompd_word_t *);
ompd_rc_t gompd_get_wait_policy (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_num_teams (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_teams_thread_limit (ompd_address_space_handle_t *,
					ompd_word_t *);
ompd_rc_t gompd_get_spin_count (ompd_address_space_handle_t *, ompd_word_t *);
ompd_rc_t gompd_get_available_cpus (ompd_address_space_handle_t *,
				    ompd_word_t *);
ompd_rc_t gompd_get_throttled_spin_count (ompd_address_space_handle_t *,
					  ompd_word_t *);
ompd_rc_t gompd_get_managed_threads (ompd_address_space_handle_t *,
				     ompd_word_t *);
ompd_rc_t gompd_stringize_gompd_enabled (ompd_address_space_handle_t *,
                                         const char **);
/*End of Global ICVs.  */


#ifdef HAVE_ATTRIBUTE_VISIBILITY
#pragma GCC visibility pop
#endif

#ifdef __cplusplus
} // extern C
#endif

#endif /* _OMPD_HELPER_H */
