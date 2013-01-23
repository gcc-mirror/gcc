/* Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* This is the default implementation of a barrier synchronization mechanism
   for libgomp.  This type is private to the library.  Note that we rely on
   being able to adjust the barrier count while threads are blocked, so the
   POSIX pthread_barrier_t won't work.  */

#include "libgomp.h"


void
gomp_barrier_init (gomp_barrier_t *bar, unsigned count)
{
  gomp_mutex_init (&bar->mutex1);
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&bar->mutex2);
#endif
  gomp_sem_init (&bar->sem1, 0);
  gomp_sem_init (&bar->sem2, 0);
  bar->total = count;
  bar->arrived = 0;
  bar->generation = 0;
}

void
gomp_barrier_destroy (gomp_barrier_t *bar)
{
  /* Before destroying, make sure all threads have left the barrier.  */
  gomp_mutex_lock (&bar->mutex1);
  gomp_mutex_unlock (&bar->mutex1);

  gomp_mutex_destroy (&bar->mutex1);
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_destroy (&bar->mutex2);
#endif
  gomp_sem_destroy (&bar->sem1);
  gomp_sem_destroy (&bar->sem2);
}

void
gomp_barrier_reinit (gomp_barrier_t *bar, unsigned count)
{
  gomp_mutex_lock (&bar->mutex1);
  bar->total = count;
  gomp_mutex_unlock (&bar->mutex1);
}

void
gomp_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  unsigned int n;

  if (state & 1)
    {
      n = --bar->arrived;
      if (n > 0)
	{
	  do
	    gomp_sem_post (&bar->sem1);
	  while (--n != 0);
	  gomp_sem_wait (&bar->sem2);
	}
      gomp_mutex_unlock (&bar->mutex1);
    }
  else
    {
      gomp_mutex_unlock (&bar->mutex1);
      gomp_sem_wait (&bar->sem1);

#ifdef HAVE_SYNC_BUILTINS
      n = __sync_add_and_fetch (&bar->arrived, -1);
#else
      gomp_mutex_lock (&bar->mutex2);
      n = --bar->arrived;
      gomp_mutex_unlock (&bar->mutex2);
#endif

      if (n == 0)
	gomp_sem_post (&bar->sem2);
    }
}

void
gomp_barrier_wait (gomp_barrier_t *barrier)
{
  gomp_barrier_wait_end (barrier, gomp_barrier_wait_start (barrier));
}

void
gomp_team_barrier_wait_end (gomp_barrier_t *bar, gomp_barrier_state_t state)
{
  unsigned int n;

  if (state & 1)
    {
      n = --bar->arrived;
      struct gomp_thread *thr = gomp_thread ();
      struct gomp_team *team = thr->ts.team;

      if (team->task_count)
	{
	  gomp_barrier_handle_tasks (state);
	  if (n > 0)
	    gomp_sem_wait (&bar->sem2);
	  gomp_mutex_unlock (&bar->mutex1);
	  return;
	}

      bar->generation = state + 3;
      if (n > 0)
	{
	  do
	    gomp_sem_post (&bar->sem1);
	  while (--n != 0);
	  gomp_sem_wait (&bar->sem2);
	}
      gomp_mutex_unlock (&bar->mutex1);
    }
  else
    {
      gomp_mutex_unlock (&bar->mutex1);
      do
	{
	  gomp_sem_wait (&bar->sem1);
	  if (bar->generation & 1)
	    gomp_barrier_handle_tasks (state);
	}
      while (bar->generation != state + 4);

#ifdef HAVE_SYNC_BUILTINS
      n = __sync_add_and_fetch (&bar->arrived, -1);
#else
      gomp_mutex_lock (&bar->mutex2);
      n = --bar->arrived;
      gomp_mutex_unlock (&bar->mutex2);
#endif

      if (n == 0)
	gomp_sem_post (&bar->sem2);
    }
}

void
gomp_team_barrier_wait (gomp_barrier_t *barrier)
{
  gomp_team_barrier_wait_end (barrier, gomp_barrier_wait_start (barrier));
}

void
gomp_team_barrier_wake (gomp_barrier_t *bar, int count)
{
  if (count == 0)
    count = bar->total - 1;
  while (count-- > 0)
    gomp_sem_post (&bar->sem1);
}
