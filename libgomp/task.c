/* Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

typedef struct gomp_task_depend_entry *hash_entry_type;

static inline void *
htab_alloc (size_t size)
{
  return gomp_malloc (size);
}

static inline void
htab_free (void *ptr)
{
  free (ptr);
}

#include "hashtab.h"

static inline hashval_t
htab_hash (hash_entry_type element)
{
  return hash_pointer (element->addr);
}

static inline bool
htab_eq (hash_entry_type x, hash_entry_type y)
{
  return x->addr == y->addr;
}

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
  task->copy_ctors_done = false;
  task->children = NULL;
  task->taskgroup = NULL;
  task->dependers = NULL;
  task->depend_hash = NULL;
  task->depend_count = 0;
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
	   long arg_size, long arg_align, bool if_clause, unsigned flags,
	   void **depend)
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

  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return;

  if (!if_clause || team == NULL
      || (thr->task && thr->task->final_task)
      || team->task_count > 64 * team->nthreads)
    {
      struct gomp_task task;

      /* If there are depend clauses and earlier deferred sibling tasks
	 with depend clauses, check if there isn't a dependency.  If there
	 is, fall through to the deferred task handling, as we can't
	 schedule such tasks right away.  There is no need to handle
	 depend clauses for non-deferred tasks other than this, because
	 the parent task is suspended until the child task finishes and thus
	 it can't start further child tasks.  */
      if ((flags & 8) && thr->task && thr->task->depend_hash)
	{
	  struct gomp_task *parent = thr->task;
	  struct gomp_task_depend_entry elem, *ent = NULL;
	  size_t ndepend = (uintptr_t) depend[0];
	  size_t nout = (uintptr_t) depend[1];
	  size_t i;
	  gomp_mutex_lock (&team->task_lock);
	  for (i = 0; i < ndepend; i++)
	    {
	      elem.addr = depend[i + 2];
	      ent = htab_find (parent->depend_hash, &elem);
	      for (; ent; ent = ent->next)
		if (i >= nout && ent->is_in)
		  continue;
		else
		  break;
	      if (ent)
		break;
	    }
	  gomp_mutex_unlock (&team->task_lock);
	  if (ent)
	    goto defer;
	}

      gomp_init_task (&task, thr->task, gomp_icv (false));
      task.kind = GOMP_TASK_IFFALSE;
      task.final_task = (thr->task && thr->task->final_task) || (flags & 2);
      if (thr->task)
	{
	  task.in_tied_task = thr->task->in_tied_task;
	  task.taskgroup = thr->task->taskgroup;
	}
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
     defer:;
      struct gomp_task *task;
      struct gomp_task *parent = thr->task;
      struct gomp_taskgroup *taskgroup = parent->taskgroup;
      char *arg;
      bool do_wake;
      size_t depend_size = 0;

      if (flags & 8)
	depend_size = ((uintptr_t) depend[0]
		       * sizeof (struct gomp_task_depend_entry));
      task = gomp_malloc (sizeof (*task) + depend_size
			  + arg_size + arg_align - 1);
      arg = (char *) (((uintptr_t) (task + 1) + depend_size + arg_align - 1)
		      & ~(uintptr_t) (arg_align - 1));
      gomp_init_task (task, parent, gomp_icv (false));
      task->kind = GOMP_TASK_IFFALSE;
      task->in_tied_task = parent->in_tied_task;
      task->taskgroup = taskgroup;
      thr->task = task;
      if (cpyfn)
	{
	  cpyfn (arg, data);
	  task->copy_ctors_done = true;
	}
      else
	memcpy (arg, data, arg_size);
      thr->task = parent;
      task->kind = GOMP_TASK_WAITING;
      task->fn = fn;
      task->fn_data = arg;
      task->final_task = (flags & 2) >> 1;
      gomp_mutex_lock (&team->task_lock);
      /* If parallel or taskgroup has been cancelled, don't start new
	 tasks.  */
      if (__builtin_expect ((gomp_team_barrier_cancelled (&team->barrier)
			     || (taskgroup && taskgroup->cancelled))
			    && !task->copy_ctors_done, 0))
	{
	  gomp_mutex_unlock (&team->task_lock);
	  gomp_finish_task (task);
	  free (task);
	  return;
	}
      if (taskgroup)
	taskgroup->num_children++;
      if (depend_size)
	{
	  size_t ndepend = (uintptr_t) depend[0];
	  size_t nout = (uintptr_t) depend[1];
	  size_t i;
	  hash_entry_type ent;

	  task->depend_count = ndepend;
	  task->num_dependees = 0;
	  if (parent->depend_hash == NULL)
	    parent->depend_hash
	      = htab_create (2 * ndepend > 12 ? 2 * ndepend : 12);
	  for (i = 0; i < ndepend; i++)
	    {
	      task->depend[i].addr = depend[2 + i];
	      task->depend[i].next = NULL;
	      task->depend[i].prev = NULL;
	      task->depend[i].task = task;
	      task->depend[i].is_in = i >= nout;
	      task->depend[i].redundant = false;

	      hash_entry_type *slot
		= htab_find_slot (&parent->depend_hash, &task->depend[i],
				  INSERT);
	      hash_entry_type out = NULL;
	      if (*slot)
		{
		  /* If multiple depends on the same task are the
		     same, all but the first one are redundant.
		     As inout/out come first, if any of them is
		     inout/out, it will win, which is the right
		     semantics.  */
		  if ((*slot)->task == task)
		    {
		      task->depend[i].redundant = true;
		      continue;
		    }
		  for (ent = *slot; ent; ent = ent->next)
		    {
		      /* depend(in:...) doesn't depend on earlier
			 depend(in:...).  */
		      if (i >= nout && ent->is_in)
			continue;

		      if (!ent->is_in)
			out = ent;

		      struct gomp_task *tsk = ent->task;
		      if (tsk->dependers == NULL)
			{
			  tsk->dependers
			    = gomp_malloc (sizeof (struct gomp_dependers_vec)
					   + 6 * sizeof (struct gomp_task *));
			  tsk->dependers->n_elem = 1;
			  tsk->dependers->allocated = 6;
			  tsk->dependers->elem[0] = task;
			  task->num_dependees++;
			  continue;
			}
		      /* We already have some other dependency on tsk
			 from earlier depend clause.  */
		      else if (tsk->dependers->n_elem
			       && (tsk->dependers->elem[tsk->dependers->n_elem
							- 1]
				   == task))
			continue;
		      else if (tsk->dependers->n_elem
			       == tsk->dependers->allocated)
			{
			  tsk->dependers->allocated
			    = tsk->dependers->allocated * 2 + 2;
			  tsk->dependers
			    = gomp_realloc (tsk->dependers,
					    sizeof (struct gomp_dependers_vec)
					    + (tsk->dependers->allocated
					       * sizeof (struct gomp_task *)));
			}
		      tsk->dependers->elem[tsk->dependers->n_elem++] = task;
		      task->num_dependees++;
		    }
		  task->depend[i].next = *slot;
		  (*slot)->prev = &task->depend[i];
		}
	      *slot = &task->depend[i];

	      /* There is no need to store more than one depend({,in}out:)
		 task per address in the hash table chain, because each out
		 depends on all earlier outs, thus it is enough to record
		 just the last depend({,in}out:).  For depend(in:), we need
		 to keep all of the previous ones not terminated yet, because
		 a later depend({,in}out:) might need to depend on all of
		 them.  So, if the new task's clause is depend({,in}out:),
		 we know there is at most one other depend({,in}out:) clause
		 in the list (out) and to maintain the invariant we now
		 need to remove it from the list.  */
	      if (!task->depend[i].is_in && out)
		{
		  if (out->next)
		    out->next->prev = out->prev;
		  out->prev->next = out->next;
		  out->redundant = true;
		}
	    }
	  if (task->num_dependees)
	    {
	      gomp_mutex_unlock (&team->task_lock);
	      return;
	    }
	}
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
      if (taskgroup)
	{
	  if (taskgroup->children)
	    {
	      task->next_taskgroup = taskgroup->children;
	      task->prev_taskgroup = taskgroup->children->prev_taskgroup;
	      task->next_taskgroup->prev_taskgroup = task;
	      task->prev_taskgroup->next_taskgroup = task;
	    }
	  else
	    {
	      task->next_taskgroup = task;
	      task->prev_taskgroup = task;
	    }
	  taskgroup->children = task;
	}
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
      ++team->task_queued_count;
      gomp_team_barrier_set_task_pending (&team->barrier);
      do_wake = team->task_running_count + !parent->in_tied_task
		< team->nthreads;
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	gomp_team_barrier_wake (&team->barrier, 1);
    }
}

static inline bool
gomp_task_run_pre (struct gomp_task *child_task, struct gomp_task *parent,
		   struct gomp_taskgroup *taskgroup, struct gomp_team *team)
{
  if (parent && parent->children == child_task)
    parent->children = child_task->next_child;
  if (taskgroup && taskgroup->children == child_task)
    taskgroup->children = child_task->next_taskgroup;
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
  if (--team->task_queued_count == 0)
    gomp_team_barrier_clear_task_pending (&team->barrier);
  if ((gomp_team_barrier_cancelled (&team->barrier)
       || (taskgroup && taskgroup->cancelled))
      && !child_task->copy_ctors_done)
    return true;
  return false;
}

static void
gomp_task_run_post_handle_depend_hash (struct gomp_task *child_task)
{
  struct gomp_task *parent = child_task->parent;
  size_t i;

  for (i = 0; i < child_task->depend_count; i++)
    if (!child_task->depend[i].redundant)
      {
	if (child_task->depend[i].next)
	  child_task->depend[i].next->prev = child_task->depend[i].prev;
	if (child_task->depend[i].prev)
	  child_task->depend[i].prev->next = child_task->depend[i].next;
	else
	  {
	    hash_entry_type *slot
	      = htab_find_slot (&parent->depend_hash, &child_task->depend[i],
				NO_INSERT);
	    if (*slot != &child_task->depend[i])
	      abort ();
	    if (child_task->depend[i].next)
	      *slot = child_task->depend[i].next;
	    else
	      htab_clear_slot (parent->depend_hash, slot);
	  }
      }
}

static size_t
gomp_task_run_post_handle_dependers (struct gomp_task *child_task,
				     struct gomp_team *team)
{
  struct gomp_task *parent = child_task->parent;
  size_t i, count = child_task->dependers->n_elem, ret = 0;
  for (i = 0; i < count; i++)
    {
      struct gomp_task *task = child_task->dependers->elem[i];
      if (--task->num_dependees != 0)
	continue;

      struct gomp_taskgroup *taskgroup = task->taskgroup;
      if (parent)
	{
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
	  if (parent->in_taskwait)
	    {
	      parent->in_taskwait = false;
	      gomp_sem_post (&parent->taskwait_sem);
	    }
	}
      if (taskgroup)
	{
	  if (taskgroup->children)
	    {
	      task->next_taskgroup = taskgroup->children;
	      task->prev_taskgroup = taskgroup->children->prev_taskgroup;
	      task->next_taskgroup->prev_taskgroup = task;
	      task->prev_taskgroup->next_taskgroup = task;
	    }
	  else
	    {
	      task->next_taskgroup = task;
	      task->prev_taskgroup = task;
	    }
	  taskgroup->children = task;
	  if (taskgroup->in_taskgroup_wait)
	    {
	      taskgroup->in_taskgroup_wait = false;
	      gomp_sem_post (&taskgroup->taskgroup_sem);
	    }
	}
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
      ++team->task_queued_count;
      ++ret;
    }
  free (child_task->dependers);
  child_task->dependers = NULL;
  if (ret > 1)
    gomp_team_barrier_set_task_pending (&team->barrier);
  return ret;
}

static inline size_t
gomp_task_run_post_handle_depend (struct gomp_task *child_task,
				  struct gomp_team *team)
{
  if (child_task->depend_count == 0)
    return 0;

  /* If parent is gone already, the hash table is freed and nothing
     will use the hash table anymore, no need to remove anything from it.  */
  if (child_task->parent != NULL)
    gomp_task_run_post_handle_depend_hash (child_task);

  if (child_task->dependers == NULL)
    return 0;

  return gomp_task_run_post_handle_dependers (child_task, team);
}

static inline void
gomp_task_run_post_remove_parent (struct gomp_task *child_task)
{
  struct gomp_task *parent = child_task->parent;
  if (parent == NULL)
    return;
  child_task->prev_child->next_child = child_task->next_child;
  child_task->next_child->prev_child = child_task->prev_child;
  if (parent->children != child_task)
    return;
  if (child_task->next_child != child_task)
    parent->children = child_task->next_child;
  else
    {
      /* We access task->children in GOMP_taskwait
	 outside of the task lock mutex region, so
	 need a release barrier here to ensure memory
	 written by child_task->fn above is flushed
	 before the NULL is written.  */
      __atomic_store_n (&parent->children, NULL, MEMMODEL_RELEASE);
      if (parent->in_taskwait)
	{
	  parent->in_taskwait = false;
	  gomp_sem_post (&parent->taskwait_sem);
	}
    }
}

static inline void
gomp_task_run_post_remove_taskgroup (struct gomp_task *child_task)
{
  struct gomp_taskgroup *taskgroup = child_task->taskgroup;
  if (taskgroup == NULL)
    return;
  child_task->prev_taskgroup->next_taskgroup = child_task->next_taskgroup;
  child_task->next_taskgroup->prev_taskgroup = child_task->prev_taskgroup;
  if (taskgroup->num_children > 1)
    --taskgroup->num_children;
  else
    {
      /* We access taskgroup->num_children in GOMP_taskgroup_end
	 outside of the task lock mutex region, so
	 need a release barrier here to ensure memory
	 written by child_task->fn above is flushed
	 before the NULL is written.  */
      __atomic_store_n (&taskgroup->num_children, 0, MEMMODEL_RELEASE);
    }
  if (taskgroup->children != child_task)
    return;
  if (child_task->next_taskgroup != child_task)
    taskgroup->children = child_task->next_taskgroup;
  else
    {
      taskgroup->children = NULL;
      if (taskgroup->in_taskgroup_wait)
	{
	  taskgroup->in_taskgroup_wait = false;
	  gomp_sem_post (&taskgroup->taskgroup_sem);
	}
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
  int do_wake = 0;

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
      bool cancelled = false;
      if (team->task_queue != NULL)
	{
	  child_task = team->task_queue;
	  cancelled = gomp_task_run_pre (child_task, child_task->parent,
					 child_task->taskgroup, team);
	  if (__builtin_expect (cancelled, 0))
	    {
	      if (to_free)
		{
		  gomp_finish_task (to_free);
		  free (to_free);
		  to_free = NULL;
		}
	      goto finish_cancelled;
	    }
	  team->task_running_count++;
	  child_task->in_tied_task = true;
	}
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	{
	  gomp_team_barrier_wake (&team->barrier, do_wake);
	  do_wake = 0;
	}
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
	 finish_cancelled:;
	  size_t new_tasks
	    = gomp_task_run_post_handle_depend (child_task, team);
	  gomp_task_run_post_remove_parent (child_task);
	  gomp_clear_parent (child_task->children);
	  gomp_task_run_post_remove_taskgroup (child_task);
	  to_free = child_task;
	  child_task = NULL;
	  if (!cancelled)
	    team->task_running_count--;
	  if (new_tasks > 1)
	    {
	      do_wake = team->nthreads - team->task_running_count;
	      if (do_wake > new_tasks)
		do_wake = new_tasks;
	    }
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
  int do_wake = 0;

  /* The acquire barrier on load of task->children here synchronizes
     with the write of a NULL in gomp_task_run_post_remove_parent.  It is
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
      bool cancelled = false;
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
	  cancelled
	    = gomp_task_run_pre (child_task, task, child_task->taskgroup,
				 team);
	  if (__builtin_expect (cancelled, 0))
	    {
	      if (to_free)
		{
		  gomp_finish_task (to_free);
		  free (to_free);
		  to_free = NULL;
		}
	      goto finish_cancelled;
	    }
	}
      else
	/* All tasks we are waiting for are already running
	   in other threads.  Wait for them.  */
	task->in_taskwait = true;
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	{
	  gomp_team_barrier_wake (&team->barrier, do_wake);
	  do_wake = 0;
	}
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
	gomp_sem_wait (&task->taskwait_sem);
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	 finish_cancelled:;
	  size_t new_tasks
	    = gomp_task_run_post_handle_depend (child_task, team);
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
	  gomp_task_run_post_remove_taskgroup (child_task);
	  to_free = child_task;
	  child_task = NULL;
	  team->task_count--;
	  if (new_tasks > 1)
	    {
	      do_wake = team->nthreads - team->task_running_count
			- !task->in_tied_task;
	      if (do_wake > new_tasks)
		do_wake = new_tasks;
	    }
	}
    }
}

/* Called when encountering a taskyield directive.  */

void
GOMP_taskyield (void)
{
  /* Nothing at the moment.  */
}

void
GOMP_taskgroup_start (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_task *task = thr->task;
  struct gomp_taskgroup *taskgroup;

  /* If team is NULL, all tasks are executed as
     GOMP_TASK_IFFALSE tasks and thus all children tasks of
     taskgroup and their descendant tasks will be finished
     by the time GOMP_taskgroup_end is called.  */
  if (team == NULL)
    return;
  taskgroup = gomp_malloc (sizeof (struct gomp_taskgroup));
  taskgroup->prev = task->taskgroup;
  taskgroup->children = NULL;
  taskgroup->in_taskgroup_wait = false;
  taskgroup->cancelled = false;
  taskgroup->num_children = 0;
  gomp_sem_init (&taskgroup->taskgroup_sem, 0);
  task->taskgroup = taskgroup;
}

void
GOMP_taskgroup_end (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_task *task = thr->task;
  struct gomp_taskgroup *taskgroup;
  struct gomp_task *child_task = NULL;
  struct gomp_task *to_free = NULL;
  int do_wake = 0;

  if (team == NULL)
    return;
  taskgroup = task->taskgroup;

  /* The acquire barrier on load of taskgroup->num_children here
     synchronizes with the write of 0 in gomp_task_run_post_remove_taskgroup.
     It is not necessary that we synchronize with other non-0 writes at
     this point, but we must ensure that all writes to memory by a
     child thread task work function are seen before we exit from
     GOMP_taskgroup_end.  */
  if (__atomic_load_n (&taskgroup->num_children, MEMMODEL_ACQUIRE) == 0)
    goto finish;

  gomp_mutex_lock (&team->task_lock);
  while (1)
    {
      bool cancelled = false;
      if (taskgroup->children == NULL)
	{
	  if (taskgroup->num_children)
	    goto do_wait;
	  gomp_mutex_unlock (&team->task_lock);
	  if (to_free)
	    {
	      gomp_finish_task (to_free);
	      free (to_free);
	    }
	  goto finish;
	}
      if (taskgroup->children->kind == GOMP_TASK_WAITING)
	{
	  child_task = taskgroup->children;
	  cancelled
	    = gomp_task_run_pre (child_task, child_task->parent, taskgroup,
				 team);
	  if (__builtin_expect (cancelled, 0))
	    {
	      if (to_free)
		{
		  gomp_finish_task (to_free);
		  free (to_free);
		  to_free = NULL;
		}
	      goto finish_cancelled;
	    }
	}
      else
	{
	 do_wait:
	  /* All tasks we are waiting for are already running
	     in other threads.  Wait for them.  */
	  taskgroup->in_taskgroup_wait = true;
	}
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	{
	  gomp_team_barrier_wake (&team->barrier, do_wake);
	  do_wake = 0;
	}
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
	gomp_sem_wait (&taskgroup->taskgroup_sem);
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	 finish_cancelled:;
	  size_t new_tasks
	    = gomp_task_run_post_handle_depend (child_task, team);
	  child_task->prev_taskgroup->next_taskgroup
	    = child_task->next_taskgroup;
	  child_task->next_taskgroup->prev_taskgroup
	    = child_task->prev_taskgroup;
	  --taskgroup->num_children;
	  if (taskgroup->children == child_task)
	    {
	      if (child_task->next_taskgroup != child_task)
		taskgroup->children = child_task->next_taskgroup;
	      else
		taskgroup->children = NULL;
	    }
	  gomp_task_run_post_remove_parent (child_task);
	  gomp_clear_parent (child_task->children);
	  to_free = child_task;
	  child_task = NULL;
	  team->task_count--;
	  if (new_tasks > 1)
	    {
	      do_wake = team->nthreads - team->task_running_count
			- !task->in_tied_task;
	      if (do_wake > new_tasks)
		do_wake = new_tasks;
	    }
	}
    }

 finish:
  task->taskgroup = taskgroup->prev;
  gomp_sem_destroy (&taskgroup->taskgroup_sem);
  free (taskgroup);
}

int
omp_in_final (void)
{
  struct gomp_thread *thr = gomp_thread ();
  return thr->task && thr->task->final_task;
}

ialias (omp_in_final)
