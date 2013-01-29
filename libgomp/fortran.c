/* Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

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
#include <limits.h>

#ifdef HAVE_ATTRIBUTE_ALIAS
/* Use internal aliases if possible.  */
# define ULP		STR1(__USER_LABEL_PREFIX__)
# define STR1(x)	STR2(x)
# define STR2(x)	#x
# define ialias_redirect(fn) \
  extern __typeof (fn) fn __asm__ (ULP "gomp_ialias_" #fn) attribute_hidden;
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
omp_set_schedule_ (const int32_t *kind, const int32_t *modifier)
{
  omp_set_schedule (*kind, *modifier);
}

void
omp_set_schedule_8_ (const int32_t *kind, const int64_t *modifier)
{
  omp_set_schedule (*kind, TO_INT (*modifier));
}

void
omp_get_schedule_ (int32_t *kind, int32_t *modifier)
{
  omp_sched_t k;
  int m;
  omp_get_schedule (&k, &m);
  *kind = k;
  *modifier = m;
}

void
omp_get_schedule_8_ (int32_t *kind, int64_t *modifier)
{
  omp_sched_t k;
  int m;
  omp_get_schedule (&k, &m);
  *kind = k;
  *modifier = m;
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
