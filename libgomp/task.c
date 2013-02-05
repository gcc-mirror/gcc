/* Copyright (C) 2007, 2008, 2009, 2011 Free Software Foundation, Inc.
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

/* This file handles the maintainence of tasks in response to task
   creation and termination.  */

#include "libgomp.h"
#include <stdlib.h>
#include <string.h>


/* Create a new task data structure.  */

void
gomp_init_task (struct gomp_task *task, struct gomp_task *parent_task,
		struct gomp_task_icv *prev_icv)
{
  task->parent = parent_task;
  task->icv = *prev_icv;
  task->kind = GOMP_TASK_IMPLICIT;
  task->in_taskwait = false;
  task->in_tied_task = false;
  task->final_task = false;
  task->children = NULL;
  gomp_sem_init (&task->taskwait_sem, 0);
}

/* Clean up a task, after completing it.  */

void
gomp_end_task (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task *task = thr->task;

  gomp_finish_task (task);
  thr->task = task->parent;
}

static inline void
gomp_clear_parent (struct gomp_task *children)
{
  struct gomp_task *task = children;

  if (task)
    do
      {
	task->parent = NULL;
	task = task->next_child;
      }
    while (task != children);
}

/* Called when encountering an explicit task directive.  If IF_CLAUSE is
   false, then we must not delay in executing the task.  If UNTIED is true,
   then the task may be executed by any member of the team.  */

void
GOMP_task (void (*fn) (void *), void *data, void (*cpyfn) (void *, void *),
	   long arg_size, long arg_align, bool if_clause, unsigned flags)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
  /* If pthread_mutex_* is used for omp_*lock*, then each task must be
     tied to one thread all the time.  This means UNTIED tasks must be
     tied and if CPYFN is non-NULL IF(0) must be forced, as CPYFN
     might be running on different thread than FN.  */
  if (cpyfn)
    if_clause = false;
  if (flags & 1)
    flags &= ~1;
#endif

  if (!if_clause || team == NULL
      || (thr->task && thr->task->final_task)
      || team->task_count > 64 * team->nthreads)
    {
      struct gomp_task task;

      gomp_init_task (&task, thr->task, gomp_icv (false));
      task.kind = GOMP_TASK_IFFALSE;
      task.final_task = (thr->task && thr->task->final_task) || (flags & 2);
      if (thr->task)
	task.in_tied_task = thr->task->in_tied_task;
      thr->task = &task;
      if (__builtin_expect (cpyfn != NULL, 0))
	{
	  char buf[arg_size + arg_align - 1];
	  char *arg = (char *) (((uintptr_t) buf + arg_align - 1)
				& ~(uintptr_t) (arg_align - 1));
	  cpyfn (arg, data);
	  fn (arg);
	}
      else
	fn (data);
      /* Access to "children" is normally done inside a task_lock
	 mutex region, but the only way this particular task.children
	 can be set is if this thread's task work function (fn)
	 creates children.  So since the setter is *this* thread, we
	 need no barriers here when testing for non-NULL.  We can have
	 task.children set by the current thread then changed by a
	 child thread, but seeing a stale non-NULL value is not a
	 problem.  Once past the task_lock acquisition, this thread
	 will see the real value of task.children.  */
      if (task.children != NULL)
	{
	  gomp_mutex_lock (&team->task_lock);
	  gomp_clear_parent (task.children);
	  gomp_mutex_unlock (&team->task_lock);
	}
      gomp_end_task ();
    }
  else
    {
      struct gomp_task *task;
      struct gomp_task *parent = thr->task;
      char *arg;
      bool do_wake;

      task = gomp_malloc (sizeof (*task) + arg_size + arg_align - 1);
      arg = (char *) (((uintptr_t) (task + 1) + arg_align - 1)
		      & ~(uintptr_t) (arg_align - 1));
      gomp_init_task (task, parent, gomp_icv (false));
      task->kind = GOMP_TASK_IFFALSE;
      task->in_tied_task = parent->in_tied_task;
      thr->task = task;
      if (cpyfn)
	cpyfn (arg, data);
      else
	memcpy (arg, data, arg_size);
      thr->task = parent;
      task->kind = GOMP_TASK_WAITING;
      task->fn = fn;
      task->fn_data = arg;
      task->in_tied_task = true;
      task->final_task = (flags & 2) >> 1;
      gomp_mutex_lock (&team->task_lock);
      if (parent->children)
	{
	  task->next_child = parent->children;
	  task->prev_child = parent->children->prev_child;
	  task->next_child->prev_child = task;
	  task->prev_child->next_child = task;
	}
      else
	{
	  task->next_child = task;
	  task->prev_child = task;
	}
      parent->children = task;
      if (team->task_queue)
	{
	  task->next_queue = team->task_queue;
	  task->prev_queue = team->task_queue->prev_queue;
	  task->next_queue->prev_queue = task;
	  task->prev_queue->next_queue = task;
	}
      else
	{
	  task->next_queue = task;
	  task->prev_queue = task;
	  team->task_queue = task;
	}
      ++team->task_count;
      gomp_team_barrier_set_task_pending (&team->barrier);
      do_wake = team->task_running_count + !parent->in_tied_task
		< team->nthreads;
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	gomp_team_barrier_wake (&team->barrier, 1);
    }
}

void
gomp_barrier_handle_tasks (gomp_barrier_state_t state)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_task *task = thr->task;
  struct gomp_task *child_task = NULL;
  struct gomp_task *to_free = NULL;

  gomp_mutex_lock (&team->task_lock);
  if (gomp_barrier_last_thread (state))
    {
      if (team->task_count == 0)
	{
	  gomp_team_barrier_done (&team->barrier, state);
	  gomp_mutex_unlock (&team->task_lock);
	  gomp_team_barrier_wake (&team->barrier, 0);
	  return;
	}
      gomp_team_barrier_set_waiting_for_tasks (&team->barrier);
    }

  while (1)
    {
      if (team->task_queue != NULL)
	{
	  struct gomp_task *parent;

	  child_task = team->task_queue;
	  parent = child_task->parent;
	  if (parent && parent->children == child_task)
	    parent->children = child_task->next_child;
	  child_task->prev_queue->next_queue = child_task->next_queue;
	  child_task->next_queue->prev_queue = child_task->prev_queue;
	  if (child_task->next_queue != child_task)
	    team->task_queue = child_task->next_queue;
	  else
	    team->task_queue = NULL;
	  child_task->kind = GOMP_TASK_TIED;
	  team->task_running_count++;
	  if (team->task_count == team->task_running_count)
	    gomp_team_barrier_clear_task_pending (&team->barrier);
	}
      gomp_mutex_unlock (&team->task_lock);
      if (to_free)
	{
	  gomp_finish_task (to_free);
	  free (to_free);
	  to_free = NULL;
	}
      if (child_task)
	{
	  thr->task = child_task;
	  child_task->fn (child_task->fn_data);
	  thr->task = task;
	}
      else
	return;
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	  struct gomp_task *parent = child_task->parent;
	  if (parent)
	    {
	      child_task->prev_child->next_child = child_task->next_child;
	      child_task->next_child->prev_child = child_task->prev_child;
	      if (parent->children == child_task)
		{
		  if (child_task->next_child != child_task)
		    parent->children = child_task->next_child;
		  else
		    {
		      /* We access task->children in GOMP_taskwait
			 outside of the task lock mutex region, so
			 need a release barrier here to ensure memory
			 written by child_task->fn above is flushed
			 before the NULL is written.  */
		      __atomic_store_n (&parent->children, NULL,
					MEMMODEL_RELEASE);
		      if (parent->in_taskwait)
			gomp_sem_post (&parent->taskwait_sem);
		    }
		}
	    }
	  gomp_clear_parent (child_task->children);
	  to_free = child_task;
	  child_task = NULL;
	  team->task_running_count--;
	  if (--team->task_count == 0
	      && gomp_team_barrier_waiting_for_tasks (&team->barrier))
	    {
	      gomp_team_barrier_done (&team->barrier, state);
	      gomp_mutex_unlock (&team->task_lock);
	      gomp_team_barrier_wake (&team->barrier, 0);
	      gomp_mutex_lock (&team->task_lock);
	    }
	}
    }
}

/* Called when encountering a taskwait directive.  */

void
GOMP_taskwait (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_task *task = thr->task;
  struct gomp_task *child_task = NULL;
  struct gomp_task *to_free = NULL;

  /* The acquire barrier on load of task->children here synchronizes
     with the write of a NULL in gomp_barrier_handle_tasks.  It is
     not necessary that we synchronize with other non-NULL writes at
     this point, but we must ensure that all writes to memory by a
     child thread task work function are seen before we exit from
     GOMP_taskwait.  */
  if (task == NULL
      || __atomic_load_n (&task->children, MEMMODEL_ACQUIRE) == NULL)
    return;

  gomp_mutex_lock (&team->task_lock);
  while (1)
    {
      if (task->children == NULL)
	{
	  gomp_mutex_unlock (&team->task_lock);
	  if (to_free)
	    {
	      gomp_finish_task (to_free);
	      free (to_free);
	    }
	  return;
	}
      if (task->children->kind == GOMP_TASK_WAITING)
	{
	  child_task = task->children;
	  task->children = child_task->next_child;
	  child_task->prev_queue->next_queue = child_task->next_queue;
	  child_task->next_queue->prev_queue = child_task->prev_queue;
	  if (team->task_queue == child_task)
	    {
	      if (child_task->next_queue != child_task)
		team->task_queue = child_task->next_queue;
	      else
		team->task_queue = NULL;
	    }
	  child_task->kind = GOMP_TASK_TIED;
	  team->task_running_count++;
	  if (team->task_count == team->task_running_count)
	    gomp_team_barrier_clear_task_pending (&team->barrier);
	}
      else
	/* All tasks we are waiting for are already running
	   in other threads.  Wait for them.  */
	task->in_taskwait = true;
      gomp_mutex_unlock (&team->task_lock);
      if (to_free)
	{
	  gomp_finish_task (to_free);
	  free (to_free);
	  to_free = NULL;
	}
      if (child_task)
	{
	  thr->task = child_task;
	  child_task->fn (child_task->fn_data);
	  thr->task = task;
	}
      else
	{
	  gomp_sem_wait (&task->taskwait_sem);
	  task->in_taskwait = false;
	  return;
	}
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	  child_task->prev_child->next_child = child_task->next_child;
	  child_task->next_child->prev_child = child_task->prev_child;
	  if (task->children == child_task)
	    {
	      if (child_task->next_child != child_task)
		task->children = child_task->next_child;
	      else
		task->children = NULL;
	    }
	  gomp_clear_parent (child_task->children);
	  to_free = child_task;
	  child_task = NULL;
	  team->task_count--;
	  team->task_running_count--;
	}
    }
}

/* Called when encountering a taskyield directive.  */

void
GOMP_taskyield (void)
{
  /* Nothing at the moment.  */
}

int
omp_in_final (void)
{
  struct gomp_thread *thr = gomp_thread ();
  return thr->task && thr->task->final_task;
}

ialias (omp_in_final)
