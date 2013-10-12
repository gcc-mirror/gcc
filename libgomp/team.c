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

/* This file handles the maintainence of threads in response to team
   creation and termination.  */

#include "libgomp.h"
#include <stdlib.h>
#include <string.h>

/* This attribute contains PTHREAD_CREATE_DETACHED.  */
pthread_attr_t gomp_thread_attr;

/* This key is for the thread destructor.  */
pthread_key_t gomp_thread_destructor;


/* This is the libgomp per-thread data structure.  */
#ifdef HAVE_TLS
__thread struct gomp_thread gomp_tls_data;
#else
pthread_key_t gomp_tls_key;
#endif


/* This structure is used to communicate across pthread_create.  */

struct gomp_thread_start_data
{
  void (*fn) (void *);
  void *fn_data;
  struct gomp_team_state ts;
  struct gomp_task *task;
  struct gomp_thread_pool *thread_pool;
  unsigned int place;
  bool nested;
};


/* This function is a pthread_create entry point.  This contains the idle
   loop in which a thread waits to be called up to become part of a team.  */

static void *
gomp_thread_start (void *xdata)
{
  struct gomp_thread_start_data *data = xdata;
  struct gomp_thread *thr;
  struct gomp_thread_pool *pool;
  void (*local_fn) (void *);
  void *local_data;

#ifdef HAVE_TLS
  thr = &gomp_tls_data;
#else
  struct gomp_thread local_thr;
  thr = &local_thr;
  pthread_setspecific (gomp_tls_key, thr);
#endif
  gomp_sem_init (&thr->release, 0);

  /* Extract what we need from data.  */
  local_fn = data->fn;
  local_data = data->fn_data;
  thr->thread_pool = data->thread_pool;
  thr->ts = data->ts;
  thr->task = data->task;
  thr->place = data->place;

  thr->ts.team->ordered_release[thr->ts.team_id] = &thr->release;

  /* Make thread pool local. */
  pool = thr->thread_pool;

  if (data->nested)
    {
      struct gomp_team *team = thr->ts.team;
      struct gomp_task *task = thr->task;

      gomp_barrier_wait (&team->barrier);

      local_fn (local_data);
      gomp_team_barrier_wait_final (&team->barrier);
      gomp_finish_task (task);
      gomp_barrier_wait_last (&team->barrier);
    }
  else
    {
      pool->threads[thr->ts.team_id] = thr;

      gomp_barrier_wait (&pool->threads_dock);
      do
	{
	  struct gomp_team *team = thr->ts.team;
	  struct gomp_task *task = thr->task;

	  local_fn (local_data);
	  gomp_team_barrier_wait_final (&team->barrier);
	  gomp_finish_task (task);

	  gomp_barrier_wait (&pool->threads_dock);

	  local_fn = thr->fn;
	  local_data = thr->data;
	  thr->fn = NULL;
	}
      while (local_fn);
    }

  gomp_sem_destroy (&thr->release);
  thr->thread_pool = NULL;
  thr->task = NULL;
  return NULL;
}


/* Create a new team data structure.  */

struct gomp_team *
gomp_new_team (unsigned nthreads)
{
  struct gomp_team *team;
  size_t size;
  int i;

  size = sizeof (*team) + nthreads * (sizeof (team->ordered_release[0])
				      + sizeof (team->implicit_task[0]));
  team = gomp_malloc (size);

  team->work_share_chunk = 8;
#ifdef HAVE_SYNC_BUILTINS
  team->single_count = 0;
#else
  gomp_mutex_init (&team->work_share_list_free_lock);
#endif
  team->work_shares_to_free = &team->work_shares[0];
  gomp_init_work_share (&team->work_shares[0], false, nthreads);
  team->work_shares[0].next_alloc = NULL;
  team->work_share_list_free = NULL;
  team->work_share_list_alloc = &team->work_shares[1];
  for (i = 1; i < 7; i++)
    team->work_shares[i].next_free = &team->work_shares[i + 1];
  team->work_shares[i].next_free = NULL;

  team->nthreads = nthreads;
  gomp_barrier_init (&team->barrier, nthreads);

  gomp_sem_init (&team->master_release, 0);
  team->ordered_release = (void *) &team->implicit_task[nthreads];
  team->ordered_release[0] = &team->master_release;

  gomp_mutex_init (&team->task_lock);
  team->task_queue = NULL;
  team->task_count = 0;
  team->task_queued_count = 0;
  team->task_running_count = 0;
  team->work_share_cancelled = 0;
  team->team_cancelled = 0;

  return team;
}


/* Free a team data structure.  */

static void
free_team (struct gomp_team *team)
{
  gomp_barrier_destroy (&team->barrier);
  gomp_mutex_destroy (&team->task_lock);
  free (team);
}

/* Allocate and initialize a thread pool. */

static struct gomp_thread_pool *gomp_new_thread_pool (void)
{
  struct gomp_thread_pool *pool
    = gomp_malloc (sizeof(struct gomp_thread_pool));
  pool->threads = NULL;
  pool->threads_size = 0;
  pool->threads_used = 0;
  pool->last_team = NULL;
  return pool;
}

static void
gomp_free_pool_helper (void *thread_pool)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool
    = (struct gomp_thread_pool *) thread_pool;
  gomp_barrier_wait_last (&pool->threads_dock);
  gomp_sem_destroy (&thr->release);
  thr->thread_pool = NULL;
  thr->task = NULL;
  pthread_exit (NULL);
}

/* Free a thread pool and release its threads. */

void
gomp_free_thread (void *arg __attribute__((unused)))
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_thread_pool *pool = thr->thread_pool;
  if (pool)
    {
      if (pool->threads_used > 0)
	{
	  int i;
	  for (i = 1; i < pool->threads_used; i++)
	    {
	      struct gomp_thread *nthr = pool->threads[i];
	      nthr->fn = gomp_free_pool_helper;
	      nthr->data = pool;
	    }
	  /* This barrier undocks threads docked on pool->threads_dock.  */
	  gomp_barrier_wait (&pool->threads_dock);
	  /* And this waits till all threads have called gomp_barrier_wait_last
	     in gomp_free_pool_helper.  */
	  gomp_barrier_wait (&pool->threads_dock);
	  /* Now it is safe to destroy the barrier and free the pool.  */
	  gomp_barrier_destroy (&pool->threads_dock);

#ifdef HAVE_SYNC_BUILTINS
	  __sync_fetch_and_add (&gomp_managed_threads,
				1L - pool->threads_used);
#else
	  gomp_mutex_lock (&gomp_managed_threads_lock);
	  gomp_managed_threads -= pool->threads_used - 1L;
	  gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
	}
      free (pool->threads);
      if (pool->last_team)
	free_team (pool->last_team);
      free (pool);
      thr->thread_pool = NULL;
    }
  if (thr->task != NULL)
    {
      struct gomp_task *task = thr->task;
      gomp_end_task ();
      free (task);
    }
}

/* Launch a team.  */

void
gomp_team_start (void (*fn) (void *), void *data, unsigned nthreads,
		 unsigned flags, struct gomp_team *team)
{
  struct gomp_thread_start_data *start_data;
  struct gomp_thread *thr, *nthr;
  struct gomp_task *task;
  struct gomp_task_icv *icv;
  bool nested;
  struct gomp_thread_pool *pool;
  unsigned i, n, old_threads_used = 0;
  pthread_attr_t thread_attr, *attr;
  unsigned long nthreads_var;
  char bind, bind_var;
  unsigned int s = 0, rest = 0, p = 0, k = 0;
  unsigned int affinity_count = 0;
  struct gomp_thread **affinity_thr = NULL;

  thr = gomp_thread ();
  nested = thr->ts.team != NULL;
  if (__builtin_expect (thr->thread_pool == NULL, 0))
    {
      thr->thread_pool = gomp_new_thread_pool ();
      thr->thread_pool->threads_busy = nthreads;
      pthread_setspecific (gomp_thread_destructor, thr);
    }
  pool = thr->thread_pool;
  task = thr->task;
  icv = task ? &task->icv : &gomp_global_icv;
  if (__builtin_expect (gomp_places_list != NULL, 0) && thr->place == 0)
    gomp_init_affinity ();

  /* Always save the previous state, even if this isn't a nested team.
     In particular, we should save any work share state from an outer
     orphaned work share construct.  */
  team->prev_ts = thr->ts;

  thr->ts.team = team;
  thr->ts.team_id = 0;
  ++thr->ts.level;
  if (nthreads > 1)
    ++thr->ts.active_level;
  thr->ts.work_share = &team->work_shares[0];
  thr->ts.last_work_share = NULL;
#ifdef HAVE_SYNC_BUILTINS
  thr->ts.single_count = 0;
#endif
  thr->ts.static_trip = 0;
  thr->task = &team->implicit_task[0];
  nthreads_var = icv->nthreads_var;
  if (__builtin_expect (gomp_nthreads_var_list != NULL, 0)
      && thr->ts.level < gomp_nthreads_var_list_len)
    nthreads_var = gomp_nthreads_var_list[thr->ts.level];
  bind_var = icv->bind_var;
  if (bind_var != omp_proc_bind_false && (flags & 7) != omp_proc_bind_false)
    bind_var = flags & 7;
  bind = bind_var;
  if (__builtin_expect (gomp_bind_var_list != NULL, 0)
      && thr->ts.level < gomp_bind_var_list_len)
    bind_var = gomp_bind_var_list[thr->ts.level];
  gomp_init_task (thr->task, task, icv);
  team->implicit_task[0].icv.nthreads_var = nthreads_var;
  team->implicit_task[0].icv.bind_var = bind_var;

  if (nthreads == 1)
    return;

  i = 1;

  if (__builtin_expect (gomp_places_list != NULL, 0))
    {
      /* Depending on chosen proc_bind model, set subpartition
	 for the master thread and initialize helper variables
	 P and optionally S, K and/or REST used by later place
	 computation for each additional thread.  */
      p = thr->place - 1;
      switch (bind)
	{
	case omp_proc_bind_true:
	case omp_proc_bind_close:
	  if (nthreads > thr->ts.place_partition_len)
	    {
	      /* T > P.  S threads will be placed in each place,
		 and the final REM threads placed one by one
		 into the already occupied places.  */
	      s = nthreads / thr->ts.place_partition_len;
	      rest = nthreads % thr->ts.place_partition_len;
	    }
	  else
	    s = 1;
	  k = 1;
	  break;
	case omp_proc_bind_master:
	  /* Each thread will be bound to master's place.  */
	  break;
	case omp_proc_bind_spread:
	  if (nthreads <= thr->ts.place_partition_len)
	    {
	      /* T <= P.  Each subpartition will have in between s
		 and s+1 places (subpartitions starting at or
		 after rest will have s places, earlier s+1 places),
		 each thread will be bound to the first place in
		 its subpartition (except for the master thread
		 that can be bound to another place in its
		 subpartition).  */
	      s = thr->ts.place_partition_len / nthreads;
	      rest = thr->ts.place_partition_len % nthreads;
	      rest = (s + 1) * rest + thr->ts.place_partition_off;
	      if (p < rest)
		{
		  p -= (p - thr->ts.place_partition_off) % (s + 1);
		  thr->ts.place_partition_len = s + 1;
		}
	      else
		{
		  p -= (p - rest) % s;
		  thr->ts.place_partition_len = s;
		}
	      thr->ts.place_partition_off = p;
	    }
	  else
	    {
	      /* T > P.  Each subpartition will have just a single
		 place and we'll place between s and s+1
		 threads into each subpartition.  */
	      s = nthreads / thr->ts.place_partition_len;
	      rest = nthreads % thr->ts.place_partition_len;
	      thr->ts.place_partition_off = p;
	      thr->ts.place_partition_len = 1;
	      k = 1;
	    }
	  break;
	}
    }
  else
    bind = omp_proc_bind_false;

  /* We only allow the reuse of idle threads for non-nested PARALLEL
     regions.  This appears to be implied by the semantics of
     threadprivate variables, but perhaps that's reading too much into
     things.  Certainly it does prevent any locking problems, since
     only the initial program thread will modify gomp_threads.  */
  if (!nested)
    {
      old_threads_used = pool->threads_used;

      if (nthreads <= old_threads_used)
	n = nthreads;
      else if (old_threads_used == 0)
	{
	  n = 0;
	  gomp_barrier_init (&pool->threads_dock, nthreads);
	}
      else
	{
	  n = old_threads_used;

	  /* Increase the barrier threshold to make sure all new
	     threads arrive before the team is released.  */
	  gomp_barrier_reinit (&pool->threads_dock, nthreads);
	}

      /* Not true yet, but soon will be.  We're going to release all
	 threads from the dock, and those that aren't part of the
	 team will exit.  */
      pool->threads_used = nthreads;

      /* If necessary, expand the size of the gomp_threads array.  It is
	 expected that changes in the number of threads are rare, thus we
	 make no effort to expand gomp_threads_size geometrically.  */
      if (nthreads >= pool->threads_size)
	{
	  pool->threads_size = nthreads + 1;
	  pool->threads
	    = gomp_realloc (pool->threads,
			    pool->threads_size
			    * sizeof (struct gomp_thread_data *));
	}

      /* Release existing idle threads.  */
      for (; i < n; ++i)
	{
	  unsigned int place_partition_off = thr->ts.place_partition_off;
	  unsigned int place_partition_len = thr->ts.place_partition_len;
	  unsigned int place = 0;
	  if (__builtin_expect (gomp_places_list != NULL, 0))
	    {
	      switch (bind)
		{
		case omp_proc_bind_true:
		case omp_proc_bind_close:
		  if (k == s)
		    {
		      ++p;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      k = 1;
		      if (i == nthreads - rest)
			s = 1;
		    }
		  else
		    ++k;
		  break;
		case omp_proc_bind_master:
		  break;
		case omp_proc_bind_spread:
		  if (k == 0)
		    {
		      /* T <= P.  */
		      if (p < rest)
			p += s + 1;
		      else
			p += s;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      place_partition_off = p;
		      if (p < rest)
			place_partition_len = s + 1;
		      else
			place_partition_len = s;
		    }
		  else
		    {
		      /* T > P.  */
		      if (k == s)
			{
			  ++p;
			  if (p == (team->prev_ts.place_partition_off
				    + team->prev_ts.place_partition_len))
			    p = team->prev_ts.place_partition_off;
			  k = 1;
			  if (i == nthreads - rest)
			    s = 1;
			}
		      else
			++k;
		      place_partition_off = p;
		      place_partition_len = 1;
		    }
		  break;
		}
	      if (affinity_thr != NULL
		  || (bind != omp_proc_bind_true
		      && pool->threads[i]->place != p + 1)
		  || pool->threads[i]->place <= place_partition_off
		  || pool->threads[i]->place > (place_partition_off
						+ place_partition_len))
		{
		  unsigned int l;
		  if (affinity_thr == NULL)
		    {
		      unsigned int j;

		      if (team->prev_ts.place_partition_len > 64)
			affinity_thr
			  = gomp_malloc (team->prev_ts.place_partition_len
					 * sizeof (struct gomp_thread *));
		      else
			affinity_thr
			  = gomp_alloca (team->prev_ts.place_partition_len
					 * sizeof (struct gomp_thread *));
		      memset (affinity_thr, '\0',
			      team->prev_ts.place_partition_len
			      * sizeof (struct gomp_thread *));
		      for (j = i; j < old_threads_used; j++)
			{
			  if (pool->threads[j]->place
			      > team->prev_ts.place_partition_off
			      && (pool->threads[j]->place
				  <= (team->prev_ts.place_partition_off
				      + team->prev_ts.place_partition_len)))
			    {
			      l = pool->threads[j]->place - 1
				  - team->prev_ts.place_partition_off;
			      pool->threads[j]->data = affinity_thr[l];
			      affinity_thr[l] = pool->threads[j];
			    }
			  pool->threads[j] = NULL;
			}
		      if (nthreads > old_threads_used)
			memset (&pool->threads[old_threads_used],
				'\0', ((nthreads - old_threads_used)
				       * sizeof (struct gomp_thread *)));
		      n = nthreads;
		      affinity_count = old_threads_used - i;
		    }
		  if (affinity_count == 0)
		    break;
		  l = p;
		  if (affinity_thr[l - team->prev_ts.place_partition_off]
		      == NULL)
		    {
		      if (bind != omp_proc_bind_true)
			continue;
		      for (l = place_partition_off;
			   l < place_partition_off + place_partition_len;
			   l++)
			if (affinity_thr[l - team->prev_ts.place_partition_off]
			    != NULL)
			  break;
		      if (l == place_partition_off + place_partition_len)
			continue;
		    }
		  nthr = affinity_thr[l - team->prev_ts.place_partition_off];
		  affinity_thr[l - team->prev_ts.place_partition_off]
		    = (struct gomp_thread *) nthr->data;
		  affinity_count--;
		  pool->threads[i] = nthr;
		}
	      else
		nthr = pool->threads[i];
	      place = p + 1;
	    }
	  else
	    nthr = pool->threads[i];
	  nthr->ts.team = team;
	  nthr->ts.work_share = &team->work_shares[0];
	  nthr->ts.last_work_share = NULL;
	  nthr->ts.team_id = i;
	  nthr->ts.level = team->prev_ts.level + 1;
	  nthr->ts.active_level = thr->ts.active_level;
	  nthr->ts.place_partition_off = place_partition_off;
	  nthr->ts.place_partition_len = place_partition_len;
#ifdef HAVE_SYNC_BUILTINS
	  nthr->ts.single_count = 0;
#endif
	  nthr->ts.static_trip = 0;
	  nthr->task = &team->implicit_task[i];
	  nthr->place = place;
	  gomp_init_task (nthr->task, task, icv);
	  team->implicit_task[i].icv.nthreads_var = nthreads_var;
	  team->implicit_task[i].icv.bind_var = bind_var;
	  nthr->fn = fn;
	  nthr->data = data;
	  team->ordered_release[i] = &nthr->release;
	}

      if (__builtin_expect (affinity_thr != NULL, 0))
	{
	  /* If AFFINITY_THR is non-NULL just because we had to
	     permute some threads in the pool, but we've managed
	     to find exactly as many old threads as we'd find
	     without affinity, we don't need to handle this
	     specially anymore.  */
	  if (nthreads <= old_threads_used
	      ? (affinity_count == old_threads_used - nthreads)
	      : (i == old_threads_used))
	    {
	      if (team->prev_ts.place_partition_len > 64)
		free (affinity_thr);
	      affinity_thr = NULL;
	      affinity_count = 0;
	    }
	  else
	    {
	      i = 1;
	      /* We are going to compute the places/subpartitions
		 again from the beginning.  So, we need to reinitialize
		 vars modified by the switch (bind) above inside
		 of the loop, to the state they had after the initial
		 switch (bind).  */
	      switch (bind)
		{
		case omp_proc_bind_true:
		case omp_proc_bind_close:
		  if (nthreads > thr->ts.place_partition_len)
		    /* T > P.  S has been changed, so needs
		       to be recomputed.  */
		    s = nthreads / thr->ts.place_partition_len;
		  k = 1;
		  p = thr->place - 1;
		  break;
		case omp_proc_bind_master:
		  /* No vars have been changed.  */
		  break;
		case omp_proc_bind_spread:
		  p = thr->ts.place_partition_off;
		  if (k != 0)
		    {
		      /* T > P.  */
		      s = nthreads / team->prev_ts.place_partition_len;
		      k = 1;
		    }
		  break;
		}

	      /* Increase the barrier threshold to make sure all new
		 threads and all the threads we're going to let die
		 arrive before the team is released.  */
	      if (affinity_count)
		gomp_barrier_reinit (&pool->threads_dock,
				     nthreads + affinity_count);
	    }
	}

      if (i == nthreads)
	goto do_release;

    }

  if (__builtin_expect (nthreads + affinity_count > old_threads_used, 0))
    {
      long diff = (long) (nthreads + affinity_count) - (long) old_threads_used;

      if (old_threads_used == 0)
	--diff;

#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, diff);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads += diff;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
    }

  attr = &gomp_thread_attr;
  if (__builtin_expect (gomp_places_list != NULL, 0))
    {
      size_t stacksize;
      pthread_attr_init (&thread_attr);
      pthread_attr_setdetachstate (&thread_attr, PTHREAD_CREATE_DETACHED);
      if (! pthread_attr_getstacksize (&gomp_thread_attr, &stacksize))
	pthread_attr_setstacksize (&thread_attr, stacksize);
      attr = &thread_attr;
    }

  start_data = gomp_alloca (sizeof (struct gomp_thread_start_data)
			    * (nthreads-i));

  /* Launch new threads.  */
  for (; i < nthreads; ++i)
    {
      pthread_t pt;
      int err;

      start_data->ts.place_partition_off = thr->ts.place_partition_off;
      start_data->ts.place_partition_len = thr->ts.place_partition_len;
      start_data->place = 0;
      if (__builtin_expect (gomp_places_list != NULL, 0))
	{
	  switch (bind)
	    {
	    case omp_proc_bind_true:
	    case omp_proc_bind_close:
	      if (k == s)
		{
		  ++p;
		  if (p == (team->prev_ts.place_partition_off
			    + team->prev_ts.place_partition_len))
		    p = team->prev_ts.place_partition_off;
		  k = 1;
		  if (i == nthreads - rest)
		    s = 1;
		}
	      else
		++k;
	      break;
	    case omp_proc_bind_master:
	      break;
	    case omp_proc_bind_spread:
	      if (k == 0)
		{
		  /* T <= P.  */
		  if (p < rest)
		    p += s + 1;
		  else
		    p += s;
		  if (p == (team->prev_ts.place_partition_off
			    + team->prev_ts.place_partition_len))
		    p = team->prev_ts.place_partition_off;
		  start_data->ts.place_partition_off = p;
		  if (p < rest)
		    start_data->ts.place_partition_len = s + 1;
		  else
		    start_data->ts.place_partition_len = s;
		}
	      else
		{
		  /* T > P.  */
		  if (k == s)
		    {
		      ++p;
		      if (p == (team->prev_ts.place_partition_off
				+ team->prev_ts.place_partition_len))
			p = team->prev_ts.place_partition_off;
		      k = 1;
		      if (i == nthreads - rest)
			s = 1;
		    }
		  else
		    ++k;
		  start_data->ts.place_partition_off = p;
		  start_data->ts.place_partition_len = 1;
		}
	      break;
	    }
	  start_data->place = p + 1;
	  if (affinity_thr != NULL && pool->threads[i] != NULL)
	    continue;
	  gomp_init_thread_affinity (attr, p);
	}

      start_data->fn = fn;
      start_data->fn_data = data;
      start_data->ts.team = team;
      start_data->ts.work_share = &team->work_shares[0];
      start_data->ts.last_work_share = NULL;
      start_data->ts.team_id = i;
      start_data->ts.level = team->prev_ts.level + 1;
      start_data->ts.active_level = thr->ts.active_level;
#ifdef HAVE_SYNC_BUILTINS
      start_data->ts.single_count = 0;
#endif
      start_data->ts.static_trip = 0;
      start_data->task = &team->implicit_task[i];
      gomp_init_task (start_data->task, task, icv);
      team->implicit_task[i].icv.nthreads_var = nthreads_var;
      team->implicit_task[i].icv.bind_var = bind_var;
      start_data->thread_pool = pool;
      start_data->nested = nested;

      err = pthread_create (&pt, attr, gomp_thread_start, start_data++);
      if (err != 0)
	gomp_fatal ("Thread creation failed: %s", strerror (err));
    }

  if (__builtin_expect (gomp_places_list != NULL, 0))
    pthread_attr_destroy (&thread_attr);

 do_release:
  gomp_barrier_wait (nested ? &team->barrier : &pool->threads_dock);

  /* Decrease the barrier threshold to match the number of threads
     that should arrive back at the end of this team.  The extra
     threads should be exiting.  Note that we arrange for this test
     to never be true for nested teams.  If AFFINITY_COUNT is non-zero,
     the barrier as well as gomp_managed_threads was temporarily
     set to NTHREADS + AFFINITY_COUNT.  For NTHREADS < OLD_THREADS_COUNT,
     AFFINITY_COUNT if non-zero will be always at least
     OLD_THREADS_COUNT - NTHREADS.  */
  if (__builtin_expect (nthreads < old_threads_used, 0)
      || __builtin_expect (affinity_count, 0))
    {
      long diff = (long) nthreads - (long) old_threads_used;

      if (affinity_count)
	diff = -affinity_count;

      gomp_barrier_reinit (&pool->threads_dock, nthreads);

#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, diff);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads += diff;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
    }
  if (__builtin_expect (affinity_thr != NULL, 0)
      && team->prev_ts.place_partition_len > 64)
    free (affinity_thr);
}


/* Terminate the current team.  This is only to be called by the master
   thread.  We assume that we must wait for the other threads.  */

void
gomp_team_end (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  /* This barrier handles all pending explicit threads.
     As #pragma omp cancel parallel might get awaited count in
     team->barrier in a inconsistent state, we need to use a different
     counter here.  */
  gomp_team_barrier_wait_final (&team->barrier);
  if (__builtin_expect (team->team_cancelled, 0))
    {
      struct gomp_work_share *ws = team->work_shares_to_free;
      do
	{
	  struct gomp_work_share *next_ws = gomp_ptrlock_get (&ws->next_ws);
	  if (next_ws == NULL)
	    gomp_ptrlock_set (&ws->next_ws, ws);
	  gomp_fini_work_share (ws);
	  ws = next_ws;
	}
      while (ws != NULL);
    }
  else
    gomp_fini_work_share (thr->ts.work_share);

  gomp_end_task ();
  thr->ts = team->prev_ts;

  if (__builtin_expect (thr->ts.team != NULL, 0))
    {
#ifdef HAVE_SYNC_BUILTINS
      __sync_fetch_and_add (&gomp_managed_threads, 1L - team->nthreads);
#else
      gomp_mutex_lock (&gomp_managed_threads_lock);
      gomp_managed_threads -= team->nthreads - 1L;
      gomp_mutex_unlock (&gomp_managed_threads_lock);
#endif
      /* This barrier has gomp_barrier_wait_last counterparts
	 and ensures the team can be safely destroyed.  */
      gomp_barrier_wait (&team->barrier);
    }

  if (__builtin_expect (team->work_shares[0].next_alloc != NULL, 0))
    {
      struct gomp_work_share *ws = team->work_shares[0].next_alloc;
      do
	{
	  struct gomp_work_share *next_ws = ws->next_alloc;
	  free (ws);
	  ws = next_ws;
	}
      while (ws != NULL);
    }
  gomp_sem_destroy (&team->master_release);
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_destroy (&team->work_share_list_free_lock);
#endif

  if (__builtin_expect (thr->ts.team != NULL, 0)
      || __builtin_expect (team->nthreads == 1, 0))
    free_team (team);
  else
    {
      struct gomp_thread_pool *pool = thr->thread_pool;
      if (pool->last_team)
	free_team (pool->last_team);
      pool->last_team = team;
    }
}


/* Constructors for this file.  */

static void __attribute__((constructor))
initialize_team (void)
{
#ifndef HAVE_TLS
  static struct gomp_thread initial_thread_tls_data;

  pthread_key_create (&gomp_tls_key, NULL);
  pthread_setspecific (gomp_tls_key, &initial_thread_tls_data);
#endif

  if (pthread_key_create (&gomp_thread_destructor, gomp_free_thread) != 0)
    gomp_fatal ("could not create thread pool destructor.");
}

static void __attribute__((destructor))
team_destructor (void)
{
  /* Without this dlclose on libgomp could lead to subsequent
     crashes.  */
  pthread_key_delete (gomp_thread_destructor);
}

struct gomp_task_icv *
gomp_new_icv (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task *task = gomp_malloc (sizeof (struct gomp_task));
  gomp_init_task (task, NULL, &gomp_global_icv);
  thr->task = task;
  pthread_setspecific (gomp_thread_destructor, thr);
  return &task->icv;
}
