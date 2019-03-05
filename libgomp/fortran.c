/* Copyright (C) 2005-2019 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

/* This file contains Fortran wrapper routines.  */

#include "libgomp.h"
#include "libgomp_f.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#ifdef HAVE_ATTRIBUTE_ALIAS
/* Use internal aliases if possible.  */
# ifndef LIBGOMP_GNU_SYMBOL_VERSIONING
ialias_redirect (omp_init_lock)
ialias_redirect (omp_init_nest_lock)
ialias_redirect (omp_destroy_lock)
ialias_redirect (omp_destroy_nest_lock)
ialias_redirect (omp_set_lock)
ialias_redirect (omp_set_nest_lock)
ialias_redirect (omp_unset_lock)
ialias_redirect (omp_unset_nest_lock)
ialias_redirect (omp_test_lock)
ialias_redirect (omp_test_nest_lock)
# endif
ialias_redirect (omp_set_dynamic)
ialias_redirect (omp_set_nested)
ialias_redirect (omp_set_num_threads)
ialias_redirect (omp_get_dynamic)
ialias_redirect (omp_get_nested)
ialias_redirect (omp_in_parallel)
ialias_redirect (omp_get_max_threads)
ialias_redirect (omp_get_num_procs)
ialias_redirect (omp_get_num_threads)
ialias_redirect (omp_get_thread_num)
ialias_redirect (omp_get_wtick)
ialias_redirect (omp_get_wtime)
ialias_redirect (omp_set_schedule)
ialias_redirect (omp_get_schedule)
ialias_redirect (omp_get_thread_limit)
ialias_redirect (omp_set_max_active_levels)
ialias_redirect (omp_get_max_active_levels)
ialias_redirect (omp_get_level)
ialias_redirect (omp_get_ancestor_thread_num)
ialias_redirect (omp_get_team_size)
ialias_redirect (omp_get_active_level)
ialias_redirect (omp_in_final)
ialias_redirect (omp_get_cancellation)
ialias_redirect (omp_get_proc_bind)
ialias_redirect (omp_get_num_places)
ialias_redirect (omp_get_place_num_procs)
ialias_redirect (omp_get_place_proc_ids)
ialias_redirect (omp_get_place_num)
ialias_redirect (omp_get_partition_num_places)
ialias_redirect (omp_get_partition_place_nums)
ialias_redirect (omp_set_default_device)
ialias_redirect (omp_get_default_device)
ialias_redirect (omp_get_num_devices)
ialias_redirect (omp_get_num_teams)
ialias_redirect (omp_get_team_num)
ialias_redirect (omp_is_initial_device)
ialias_redirect (omp_get_initial_device)
ialias_redirect (omp_get_max_task_priority)
ialias_redirect (omp_pause_resource)
ialias_redirect (omp_pause_resource_all)
#endif

#ifndef LIBGOMP_GNU_SYMBOL_VERSIONING
# define gomp_init_lock__30 omp_init_lock_
# define gomp_destroy_lock__30 omp_destroy_lock_
# define gomp_set_lock__30 omp_set_lock_
# define gomp_unset_lock__30 omp_unset_lock_
# define gomp_test_lock__30 omp_test_lock_
# define gomp_init_nest_lock__30 omp_init_nest_lock_
# define gomp_destroy_nest_lock__30 omp_destroy_nest_lock_
# define gomp_set_nest_lock__30 omp_set_nest_lock_
# define gomp_unset_nest_lock__30 omp_unset_nest_lock_
# define gomp_test_nest_lock__30 omp_test_nest_lock_
#endif

void
gomp_init_lock__30 (omp_lock_arg_t lock)
{
#ifndef OMP_LOCK_DIRECT
  omp_lock_arg (lock) = malloc (sizeof (omp_lock_t));
#endif
  gomp_init_lock_30 (omp_lock_arg (lock));
}

void
gomp_init_nest_lock__30 (omp_nest_lock_arg_t lock)
{
#ifndef OMP_NEST_LOCK_DIRECT
  omp_nest_lock_arg (lock) = malloc (sizeof (omp_nest_lock_t));
#endif
  gomp_init_nest_lock_30 (omp_nest_lock_arg (lock));
}

void
gomp_destroy_lock__30 (omp_lock_arg_t lock)
{
  gomp_destroy_lock_30 (omp_lock_arg (lock));
#ifndef OMP_LOCK_DIRECT
  free (omp_lock_arg (lock));
  omp_lock_arg (lock) = NULL;
#endif
}

void
gomp_destroy_nest_lock__30 (omp_nest_lock_arg_t lock)
{
  gomp_destroy_nest_lock_30 (omp_nest_lock_arg (lock));
#ifndef OMP_NEST_LOCK_DIRECT
  free (omp_nest_lock_arg (lock));
  omp_nest_lock_arg (lock) = NULL;
#endif
}

void
gomp_set_lock__30 (omp_lock_arg_t lock)
{
  gomp_set_lock_30 (omp_lock_arg (lock));
}

void
gomp_set_nest_lock__30 (omp_nest_lock_arg_t lock)
{
  gomp_set_nest_lock_30 (omp_nest_lock_arg (lock));
}

void
gomp_unset_lock__30 (omp_lock_arg_t lock)
{
  gomp_unset_lock_30 (omp_lock_arg (lock));
}

void
gomp_unset_nest_lock__30 (omp_nest_lock_arg_t lock)
{
  gomp_unset_nest_lock_30 (omp_nest_lock_arg (lock));
}

int32_t
gomp_test_lock__30 (omp_lock_arg_t lock)
{
  return gomp_test_lock_30 (omp_lock_arg (lock));
}

int32_t
gomp_test_nest_lock__30 (omp_nest_lock_arg_t lock)
{
  return gomp_test_nest_lock_30 (omp_nest_lock_arg (lock));
}

#ifdef LIBGOMP_GNU_SYMBOL_VERSIONING
void
gomp_init_lock__25 (omp_lock_25_arg_t lock)
{
#ifndef OMP_LOCK_25_DIRECT
  omp_lock_25_arg (lock) = malloc (sizeof (omp_lock_25_t));
#endif
  gomp_init_lock_25 (omp_lock_25_arg (lock));
}

void
gomp_init_nest_lock__25 (omp_nest_lock_25_arg_t lock)
{
#ifndef OMP_NEST_LOCK_25_DIRECT
  omp_nest_lock_25_arg (lock) = malloc (sizeof (omp_nest_lock_25_t));
#endif
  gomp_init_nest_lock_25 (omp_nest_lock_25_arg (lock));
}

void
gomp_destroy_lock__25 (omp_lock_25_arg_t lock)
{
  gomp_destroy_lock_25 (omp_lock_25_arg (lock));
#ifndef OMP_LOCK_25_DIRECT
  free (omp_lock_25_arg (lock));
  omp_lock_25_arg (lock) = NULL;
#endif
}

void
gomp_destroy_nest_lock__25 (omp_nest_lock_25_arg_t lock)
{
  gomp_destroy_nest_lock_25 (omp_nest_lock_25_arg (lock));
#ifndef OMP_NEST_LOCK_25_DIRECT
  free (omp_nest_lock_25_arg (lock));
  omp_nest_lock_25_arg (lock) = NULL;
#endif
}

void
gomp_set_lock__25 (omp_lock_25_arg_t lock)
{
  gomp_set_lock_25 (omp_lock_25_arg (lock));
}

void
gomp_set_nest_lock__25 (omp_nest_lock_25_arg_t lock)
{
  gomp_set_nest_lock_25 (omp_nest_lock_25_arg (lock));
}

void
gomp_unset_lock__25 (omp_lock_25_arg_t lock)
{
  gomp_unset_lock_25 (omp_lock_25_arg (lock));
}

void
gomp_unset_nest_lock__25 (omp_nest_lock_25_arg_t lock)
{
  gomp_unset_nest_lock_25 (omp_nest_lock_25_arg (lock));
}

int32_t
gomp_test_lock__25 (omp_lock_25_arg_t lock)
{
  return gomp_test_lock_25 (omp_lock_25_arg (lock));
}

int32_t
gomp_test_nest_lock__25 (omp_nest_lock_25_arg_t lock)
{
  return gomp_test_nest_lock_25 (omp_nest_lock_25_arg (lock));
}

omp_lock_symver (omp_init_lock_)
omp_lock_symver (omp_destroy_lock_)
omp_lock_symver (omp_set_lock_)
omp_lock_symver (omp_unset_lock_)
omp_lock_symver (omp_test_lock_)
omp_lock_symver (omp_init_nest_lock_)
omp_lock_symver (omp_destroy_nest_lock_)
omp_lock_symver (omp_set_nest_lock_)
omp_lock_symver (omp_unset_nest_lock_)
omp_lock_symver (omp_test_nest_lock_)
#endif

#define TO_INT(x) ((x) > INT_MIN ? (x) < INT_MAX ? (x) : INT_MAX : INT_MIN)

void
omp_set_dynamic_ (const int32_t *set)
{
  omp_set_dynamic (*set);
}

void
omp_set_dynamic_8_ (const int64_t *set)
{
  omp_set_dynamic (!!*set);
}

void
omp_set_nested_ (const int32_t *set)
{
  omp_set_nested (*set);
}

void
omp_set_nested_8_ (const int64_t *set)
{
  omp_set_nested (!!*set);
}

void
omp_set_num_threads_ (const int32_t *set)
{
  omp_set_num_threads (*set);
}

void
omp_set_num_threads_8_ (const int64_t *set)
{
  omp_set_num_threads (TO_INT (*set));
}

int32_t
omp_get_dynamic_ (void)
{
  return omp_get_dynamic ();
}

int32_t
omp_get_nested_ (void)
{
  return omp_get_nested ();
}

int32_t
omp_in_parallel_ (void)
{
  return omp_in_parallel ();
}

int32_t
omp_get_max_threads_ (void)
{
  return omp_get_max_threads ();
}

int32_t
omp_get_num_procs_ (void)
{
  return omp_get_num_procs ();
}

int32_t
omp_get_num_threads_ (void)
{
  return omp_get_num_threads ();
}

int32_t
omp_get_thread_num_ (void)
{
  return omp_get_thread_num ();
}

double
omp_get_wtick_ (void)
{
  return omp_get_wtick ();
}

double
omp_get_wtime_ (void)
{
  return omp_get_wtime ();
}

void
omp_set_schedule_ (const int32_t *kind, const int32_t *chunk_size)
{
  omp_set_schedule (*kind, *chunk_size);
}

void
omp_set_schedule_8_ (const int32_t *kind, const int64_t *chunk_size)
{
  omp_set_schedule (*kind, TO_INT (*chunk_size));
}

void
omp_get_schedule_ (int32_t *kind, int32_t *chunk_size)
{
  omp_sched_t k;
  int cs;
  omp_get_schedule (&k, &cs);
  /* For now mask off GFS_MONOTONIC, because OpenMP 4.5 code will not
     expect to see it.  */
  *kind = k & ~GFS_MONOTONIC;
  *chunk_size = cs;
}

void
omp_get_schedule_8_ (int32_t *kind, int64_t *chunk_size)
{
  omp_sched_t k;
  int cs;
  omp_get_schedule (&k, &cs);
  /* See above.  */
  *kind = k & ~GFS_MONOTONIC;
  *chunk_size = cs;
}

int32_t
omp_get_thread_limit_ (void)
{
  return omp_get_thread_limit ();
}

void
omp_set_max_active_levels_ (const int32_t *levels)
{
  omp_set_max_active_levels (*levels);
}

void
omp_set_max_active_levels_8_ (const int64_t *levels)
{
  omp_set_max_active_levels (TO_INT (*levels));
}

int32_t
omp_get_max_active_levels_ (void)
{
  return omp_get_max_active_levels ();
}

int32_t
omp_get_level_ (void)
{
  return omp_get_level ();
}

int32_t
omp_get_ancestor_thread_num_ (const int32_t *level)
{
  return omp_get_ancestor_thread_num (*level);
}

int32_t
omp_get_ancestor_thread_num_8_ (const int64_t *level)
{
  return omp_get_ancestor_thread_num (TO_INT (*level));
}

int32_t
omp_get_team_size_ (const int32_t *level)
{
  return omp_get_team_size (*level);
}

int32_t
omp_get_team_size_8_ (const int64_t *level)
{
  return omp_get_team_size (TO_INT (*level));
}

int32_t
omp_get_active_level_ (void)
{
  return omp_get_active_level ();
}

int32_t
omp_in_final_ (void)
{
  return omp_in_final ();
}

int32_t
omp_get_cancellation_ (void)
{
  return omp_get_cancellation ();
}

int32_t
omp_get_proc_bind_ (void)
{
  return omp_get_proc_bind ();
}

int32_t
omp_get_num_places_ (void)
{
  return omp_get_num_places ();
}

int32_t
omp_get_place_num_procs_ (const int32_t *place_num)
{
  return omp_get_place_num_procs (*place_num);
}

int32_t
omp_get_place_num_procs_8_ (const int64_t *place_num)
{
  return omp_get_place_num_procs (TO_INT (*place_num));
}

void
omp_get_place_proc_ids_ (const int32_t *place_num, int32_t *ids)
{
  omp_get_place_proc_ids (*place_num, (int *) ids);
}

void
omp_get_place_proc_ids_8_ (const int64_t *place_num, int64_t *ids)
{
  gomp_get_place_proc_ids_8 (TO_INT (*place_num), ids);
}

int32_t
omp_get_place_num_ (void)
{
  return omp_get_place_num ();
}

int32_t
omp_get_partition_num_places_ (void)
{
  return omp_get_partition_num_places ();
}

void
omp_get_partition_place_nums_ (int32_t *place_nums)
{
  omp_get_partition_place_nums ((int *) place_nums);
}

void
omp_get_partition_place_nums_8_ (int64_t *place_nums)
{
  if (gomp_places_list == NULL)
    return;

  struct gomp_thread *thr = gomp_thread ();
  if (thr->place == 0)
    gomp_init_affinity ();

  unsigned int i;
  for (i = 0; i < thr->ts.place_partition_len; i++)
    *place_nums++ = (int64_t) thr->ts.place_partition_off + i;
}

void
omp_set_default_device_ (const int32_t *device_num)
{
  return omp_set_default_device (*device_num);
}

void
omp_set_default_device_8_ (const int64_t *device_num)
{
  return omp_set_default_device (TO_INT (*device_num));
}

int32_t
omp_get_default_device_ (void)
{
  return omp_get_default_device ();
}

int32_t
omp_get_num_devices_ (void)
{
  return omp_get_num_devices ();
}

int32_t
omp_get_num_teams_ (void)
{
  return omp_get_num_teams ();
}

int32_t
omp_get_team_num_ (void)
{
  return omp_get_team_num ();
}

int32_t
omp_is_initial_device_ (void)
{
  return omp_is_initial_device ();
}

int32_t
omp_get_initial_device_ (void)
{
  return omp_get_initial_device ();
}

int32_t
omp_get_max_task_priority_ (void)
{
  return omp_get_max_task_priority ();
}

void
omp_set_affinity_format_ (const char *format, size_t format_len)
{
  gomp_set_affinity_format (format, format_len);
}

int32_t
omp_get_affinity_format_ (char *buffer, size_t buffer_len)
{
  size_t len = strlen (gomp_affinity_format_var);
  if (buffer_len)
    {
      if (len < buffer_len)
	{
	  memcpy (buffer, gomp_affinity_format_var, len);
	  memset (buffer + len, ' ', buffer_len - len);
	}
      else
	memcpy (buffer, gomp_affinity_format_var, buffer_len);
    }
  return len;
}

void
omp_display_affinity_ (const char *format, size_t format_len)
{
  char *fmt = NULL, fmt_buf[256];
  char buf[512];
  if (format_len)
    {
      fmt = format_len < 256 ? fmt_buf : gomp_malloc (format_len + 1);
      memcpy (fmt, format, format_len);
      fmt[format_len] = '\0';
    }
  struct gomp_thread *thr = gomp_thread ();
  size_t ret
    = gomp_display_affinity (buf, sizeof buf,
			     format_len ? fmt : gomp_affinity_format_var,
			     gomp_thread_self (), &thr->ts, thr->place);
  if (ret < sizeof buf)
    {
      buf[ret] = '\n';
      gomp_print_string (buf, ret + 1);
    }
  else
    {
      char *b = gomp_malloc (ret + 1);
      gomp_display_affinity (buf, sizeof buf,
			     format_len ? fmt : gomp_affinity_format_var,
			     gomp_thread_self (), &thr->ts, thr->place);
      b[ret] = '\n';
      gomp_print_string (b, ret + 1);
      free (b);
    }
  if (fmt && fmt != fmt_buf)
    free (fmt);
}

int32_t
omp_capture_affinity_ (char *buffer, const char *format,
		       size_t buffer_len, size_t format_len)
{
  char *fmt = NULL, fmt_buf[256];
  if (format_len)
    {
      fmt = format_len < 256 ? fmt_buf : gomp_malloc (format_len + 1);
      memcpy (fmt, format, format_len);
      fmt[format_len] = '\0';
    }
  struct gomp_thread *thr = gomp_thread ();
  size_t ret
    = gomp_display_affinity (buffer, buffer_len,
			     format_len ? fmt : gomp_affinity_format_var,
			     gomp_thread_self (), &thr->ts, thr->place);
  if (fmt && fmt != fmt_buf)
    free (fmt);
  if (ret < buffer_len)
    memset (buffer + ret, ' ', buffer_len - ret);
  return ret;
}

int32_t
omp_pause_resource_ (const int32_t *kind, const int32_t *device_num)
{
  return omp_pause_resource (*kind, *device_num);
}

int32_t
omp_pause_resource_all_ (const int32_t *kind)
{
  return omp_pause_resource_all (*kind);
}
