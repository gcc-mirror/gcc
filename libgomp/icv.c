/* Copyright (C) 2005-2025 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This file defines the OpenMP API entry points that operate on internal
   control variables.  */

#include "libgomp.h"
#include "gomp-constants.h"
#include <limits.h>

ialias_redirect (omp_get_active_level)

void
omp_set_num_threads (int n)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->nthreads_var = (n > 0 ? n : 1);
}

void
omp_set_dynamic (int val)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->dyn_var = val;
}

int
omp_get_dynamic (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->dyn_var;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
void
omp_set_nested (int val)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  if (val)
    icv->max_active_levels_var = gomp_supported_active_levels;
  else if (icv->max_active_levels_var > 1)
    icv->max_active_levels_var = 1;
}

int
omp_get_nested (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return (icv->max_active_levels_var > 1
	  && icv->max_active_levels_var > omp_get_active_level ());
}
#pragma GCC diagnostic pop

void
omp_set_schedule (omp_sched_t kind, int chunk_size)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  switch (kind & ~omp_sched_monotonic)
    {
    case omp_sched_static:
      if (chunk_size < 1)
	chunk_size = 0;
      icv->run_sched_chunk_size = chunk_size;
      break;
    case omp_sched_dynamic:
    case omp_sched_guided:
      if (chunk_size < 1)
	chunk_size = 1;
      icv->run_sched_chunk_size = chunk_size;
      break;
    case omp_sched_auto:
      break;
    default:
      return;
    }
  icv->run_sched_var = kind;
}

void
omp_get_schedule (omp_sched_t *kind, int *chunk_size)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  *kind = icv->run_sched_var;
  *chunk_size = icv->run_sched_chunk_size;
}

int
omp_get_max_threads (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->nthreads_var;
}

int
omp_get_thread_limit (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->thread_limit_var > INT_MAX ? INT_MAX : icv->thread_limit_var;
}

void
omp_set_max_active_levels (int max_levels)
{
  if (max_levels >= 0)
    {
      struct gomp_task_icv *icv = gomp_icv (true);

      if (max_levels <= gomp_supported_active_levels)
	icv->max_active_levels_var = max_levels;
      else
	icv->max_active_levels_var = gomp_supported_active_levels;
    }
}

int
omp_get_max_active_levels (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->max_active_levels_var;
}

int
omp_get_supported_active_levels (void)
{
  return gomp_supported_active_levels;
}

int
omp_get_cancellation (void)
{
  return gomp_cancel_var;
}

int
omp_get_max_task_priority (void)
{
  return gomp_max_task_priority_var;
}

omp_proc_bind_t
omp_get_proc_bind (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->bind_var;
}

int
omp_get_num_places (void)
{
  return gomp_places_list_len;
}

int
omp_get_place_num (void)
{
  if (gomp_places_list == NULL)
    return -1;

  struct gomp_thread *thr = gomp_thread ();
  if (thr->place == 0)
    gomp_init_affinity ();

  return (int) thr->place - 1;
}

int
omp_get_partition_num_places (void)
{
  if (gomp_places_list == NULL)
    return 0;

  struct gomp_thread *thr = gomp_thread ();
  if (thr->place == 0)
    gomp_init_affinity ();

  return thr->ts.place_partition_len;
}

void
omp_get_partition_place_nums (int *place_nums)
{
  if (gomp_places_list == NULL)
    return;

  struct gomp_thread *thr = gomp_thread ();
  if (thr->place == 0)
    gomp_init_affinity ();

  unsigned int i;
  for (i = 0; i < thr->ts.place_partition_len; i++)
    *place_nums++ = thr->ts.place_partition_off + i;
}

void
omp_set_default_allocator (omp_allocator_handle_t allocator)
{
  struct gomp_thread *thr = gomp_thread ();
  if (allocator == omp_null_allocator)
    allocator = omp_default_mem_alloc;
  thr->ts.def_allocator = (uintptr_t) allocator;
}

omp_allocator_handle_t
omp_get_default_allocator (void)
{
  struct gomp_thread *thr = gomp_thread ();
  if (thr->ts.def_allocator == omp_null_allocator)
    return (omp_allocator_handle_t) gomp_def_allocator;
  else
    return (omp_allocator_handle_t) thr->ts.def_allocator;
}

ialias (omp_set_dynamic)
ialias (omp_get_dynamic)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
ialias (omp_set_nested)
ialias (omp_get_nested)
#pragma GCC diagnostic pop
ialias (omp_set_num_threads)
ialias (omp_set_schedule)
ialias (omp_get_schedule)
ialias (omp_get_max_threads)
ialias (omp_get_thread_limit)
ialias (omp_set_max_active_levels)
ialias (omp_get_max_active_levels)
ialias (omp_get_supported_active_levels)
ialias (omp_get_cancellation)
ialias (omp_get_proc_bind)
ialias (omp_get_max_task_priority)
ialias (omp_get_num_places)
ialias (omp_get_place_num)
ialias (omp_get_partition_num_places)
ialias (omp_get_partition_place_nums)
ialias (omp_set_default_allocator)
ialias (omp_get_default_allocator)
