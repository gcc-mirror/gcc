/* Copyright (C) 2007-2015 Free Software Foundation, Inc.
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

/* This file handles the maintainence of tasks in response to task
   creation and termination.  */

#include "libgomp.h"
#include <stdlib.h>
#include <string.h>
#include "gomp-constants.h"

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
  task->taskwait = NULL;
  task->in_tied_task = false;
  task->final_task = false;
  task->copy_ctors_done = false;
  task->parent_depends_on = false;
  task->children = NULL;
  task->taskgroup = NULL;
  task->dependers = NULL;
  task->depend_hash = NULL;
  task->depend_count = 0;
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

/* Orphan the task in CHILDREN and all its siblings.  */

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

/* Helper function for GOMP_task and gomp_create_target_task.  Depend clause
   handling for undeferred task creation.  */

static void
gomp_task_handle_depend (struct gomp_task *task, struct gomp_task *parent,
			 void **depend)
{
  size_t ndepend = (uintptr_t) depend[0];
  size_t nout = (uintptr_t) depend[1];
  size_t i;
  hash_entry_type ent;

  task->depend_count = ndepend;
  task->num_dependees = 0;
  if (parent->depend_hash == NULL)
    parent->depend_hash = htab_create (2 * ndepend > 12 ? 2 * ndepend : 12);
  for (i = 0; i < ndepend; i++)
    {
      task->depend[i].addr = depend[2 + i];
      task->depend[i].next = NULL;
      task->depend[i].prev = NULL;
      task->depend[i].task = task;
      task->depend[i].is_in = i >= nout;
      task->depend[i].redundant = false;
      task->depend[i].redundant_out = false;

      hash_entry_type *slot = htab_find_slot (&parent->depend_hash,
					      &task->depend[i], INSERT);
      hash_entry_type out = NULL, last = NULL;
      if (*slot)
	{
	  /* If multiple depends on the same task are the same, all but the
	     first one are redundant.  As inout/out come first, if any of them
	     is inout/out, it will win, which is the right semantics.  */
	  if ((*slot)->task == task)
	    {
	      task->depend[i].redundant = true;
	      continue;
	    }
	  for (ent = *slot; ent; ent = ent->next)
	    {
	      if (ent->redundant_out)
		break;

	      last = ent;

	      /* depend(in:...) doesn't depend on earlier depend(in:...).  */
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
	      /* We already have some other dependency on tsk from earlier
		 depend clause.  */
	      else if (tsk->dependers->n_elem
		       && (tsk->dependers->elem[tsk->dependers->n_elem - 1]
			   == task))
		continue;
	      else if (tsk->dependers->n_elem == tsk->dependers->allocated)
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

      /* There is no need to store more than one depend({,in}out:) task per
	 address in the hash table chain for the purpose of creation of
	 deferred tasks, because each out depends on all earlier outs, thus it
	 is enough to record just the last depend({,in}out:).  For depend(in:),
	 we need to keep all of the previous ones not terminated yet, because
	 a later depend({,in}out:) might need to depend on all of them.  So, if
	 the new task's clause is depend({,in}out:), we know there is at most
	 one other depend({,in}out:) clause in the list (out).  For
	 non-deferred tasks we want to see all outs, so they are moved to the
	 end of the chain, after first redundant_out entry all following
	 entries should be redundant_out.  */
      if (!task->depend[i].is_in && out)
	{
	  if (out != last)
	    {
	      out->next->prev = out->prev;
	      out->prev->next = out->next;
	      out->next = last->next;
	      out->prev = last;
	      last->next = out;
	      if (out->next)
		out->next->prev = out;
	    }
	  out->redundant_out = true;
	}
    }
}

/* Called when encountering an explicit task directive.  If IF_CLAUSE is
   false, then we must not delay in executing the task.  If UNTIED is true,
   then the task may be executed by any member of the team.

   DEPEND is an array containing:
	depend[0]: number of depend elements.
	depend[1]: number of depend elements of type "out".
	depend[2..N+1]: address of [1..N]th depend element.  */

void
GOMP_task (void (*fn) (void *), void *data, void (*cpyfn) (void *, void *),
	   long arg_size, long arg_align, bool if_clause, unsigned flags,
	   void **depend, int priority)
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
  flags &= ~GOMP_TASK_FLAG_UNTIED;
#endif

  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return;

  if ((flags & GOMP_TASK_FLAG_PRIORITY) == 0)
    priority = 0;
  /* FIXME, use priority.  */
  (void) priority;

  if (!if_clause || team == NULL
      || (thr->task && thr->task->final_task)
      || team->task_count > 64 * team->nthreads)
    {
      struct gomp_task task;

      /* If there are depend clauses and earlier deferred sibling tasks
	 with depend clauses, check if there isn't a dependency.  If there
	 is, we need to wait for them.  There is no need to handle
	 depend clauses for non-deferred tasks other than this, because
	 the parent task is suspended until the child task finishes and thus
	 it can't start further child tasks.  */
      if ((flags & GOMP_TASK_FLAG_DEPEND)
	  && thr->task && thr->task->depend_hash)
	gomp_task_maybe_wait_for_dependencies (depend);

      gomp_init_task (&task, thr->task, gomp_icv (false));
      task.kind = GOMP_TASK_UNDEFERRED;
      task.final_task = (thr->task && thr->task->final_task)
			|| (flags & GOMP_TASK_FLAG_FINAL);
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
      struct gomp_task *task;
      struct gomp_task *parent = thr->task;
      struct gomp_taskgroup *taskgroup = parent->taskgroup;
      char *arg;
      bool do_wake;
      size_t depend_size = 0;

      if (flags & GOMP_TASK_FLAG_DEPEND)
	depend_size = ((uintptr_t) depend[0]
		       * sizeof (struct gomp_task_depend_entry));
      task = gomp_malloc (sizeof (*task) + depend_size
			  + arg_size + arg_align - 1);
      arg = (char *) (((uintptr_t) (task + 1) + depend_size + arg_align - 1)
		      & ~(uintptr_t) (arg_align - 1));
      gomp_init_task (task, parent, gomp_icv (false));
      task->kind = GOMP_TASK_UNDEFERRED;
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
      task->final_task = (flags & GOMP_TASK_FLAG_FINAL) >> 1;
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
	  gomp_task_handle_depend (task, parent, depend);
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
	  /* If applicable, place task into its taskgroup.  */
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

ialias (GOMP_taskgroup_start)
ialias (GOMP_taskgroup_end)

#define TYPE long
#define UTYPE unsigned long
#define TYPE_is_long 1
#include "taskloop.c"
#undef TYPE
#undef UTYPE
#undef TYPE_is_long

#define TYPE unsigned long long
#define UTYPE TYPE
#define GOMP_taskloop GOMP_taskloop_ull
#include "taskloop.c"
#undef TYPE
#undef UTYPE
#undef GOMP_taskloop

/* Called for nowait target tasks.  */

void
gomp_create_target_task (struct gomp_device_descr *devicep,
			 void (*fn) (void *), size_t mapnum, void **hostaddrs,
			 size_t *sizes, unsigned short *kinds,
			 unsigned int flags, void **depend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return;

  struct gomp_target_task *ttask;
  struct gomp_task *task;
  struct gomp_task *parent = thr->task;
  struct gomp_taskgroup *taskgroup = parent->taskgroup;
  bool do_wake;
  size_t depend_size = 0;

  if (depend != NULL)
    depend_size = ((uintptr_t) depend[0]
		   * sizeof (struct gomp_task_depend_entry));
  task = gomp_malloc (sizeof (*task) + depend_size
		      + sizeof (*ttask)
		      + mapnum * (sizeof (void *) + sizeof (size_t)
				  + sizeof (unsigned short)));
  gomp_init_task (task, parent, gomp_icv (false));
  task->kind = GOMP_TASK_WAITING;
  task->in_tied_task = parent->in_tied_task;
  task->taskgroup = taskgroup;
  ttask = (struct gomp_target_task *) &task->depend[(uintptr_t) depend[0]];
  ttask->devicep = devicep;
  ttask->fn = fn;
  ttask->mapnum = mapnum;
  memcpy (ttask->hostaddrs, hostaddrs, mapnum * sizeof (void *));
  ttask->sizes = (size_t *) &ttask->hostaddrs[mapnum];
  memcpy (ttask->sizes, sizes, mapnum * sizeof (size_t));
  ttask->kinds = (unsigned short *) &ttask->sizes[mapnum];
  memcpy (ttask->kinds, kinds, mapnum * sizeof (unsigned short));
  ttask->flags = flags;
  task->fn = gomp_target_task_fn;
  task->fn_data = ttask;
  task->final_task = 0;
  gomp_mutex_lock (&team->task_lock);
  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (__builtin_expect (gomp_team_barrier_cancelled (&team->barrier)
			|| (taskgroup && taskgroup->cancelled), 0))
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
      gomp_task_handle_depend (task, parent, depend);
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
      /* If applicable, place task into its taskgroup.  */
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

#if _LIBGOMP_CHECKING
/* Sanity check TASK to make sure it is in its parent's children
   queue, and that the tasks therein are in the right order.

   The expected order is:
	parent_depends_on WAITING tasks
	!parent_depends_on WAITING tasks
	TIED tasks

   PARENT is the alleged parent of TASK.  */

static void
verify_children_queue (struct gomp_task *task, struct gomp_task *parent)
{
  if (task->parent != parent)
    gomp_fatal ("verify_children_queue: incompatible parents");
  /* It's OK, Annie was an orphan and she turned out all right.  */
  if (!parent)
    return;

  bool seen_tied = false;
  bool seen_plain_waiting = false;
  bool found = false;
  struct gomp_task *t = parent->children;
  while (1)
    {
      if (t == task)
	found = true;
      if (seen_tied && t->kind == GOMP_TASK_WAITING)
	gomp_fatal ("verify_children_queue: WAITING task after TIED");
      if (t->kind == GOMP_TASK_TIED)
	seen_tied = true;
      else if (t->kind == GOMP_TASK_WAITING)
	{
	  if (t->parent_depends_on)
	    {
	      if (seen_plain_waiting)
		gomp_fatal ("verify_children_queue: parent_depends_on after "
			    "!parent_depends_on");
	    }
	  else
	    seen_plain_waiting = true;
	}
      t = t->next_child;
      if (t == parent->children)
	break;
    }
  if (!found)
    gomp_fatal ("verify_children_queue: child not found in parent queue");
}

/* Sanity check TASK to make sure it is in its taskgroup queue (if
   applicable), and that the tasks therein are in the right order.

   The expected order is that GOMP_TASK_WAITING tasks must come before
   GOMP_TASK_TIED tasks.

   TASK is the task.  */

static void
verify_taskgroup_queue (struct gomp_task *task)
{
  struct gomp_taskgroup *taskgroup = task->taskgroup;
  if (!taskgroup)
    return;

  bool seen_tied = false;
  bool found = false;
  struct gomp_task *t = taskgroup->children;
  while (1)
    {
      if (t == task)
	found = true;
      if (t->kind == GOMP_TASK_WAITING && seen_tied)
	gomp_fatal ("verify_taskgroup_queue: WAITING task after TIED");
      if (t->kind == GOMP_TASK_TIED)
	seen_tied = true;
      t = t->next_taskgroup;
      if (t == taskgroup->children)
	break;
    }
  if (!found)
    gomp_fatal ("verify_taskgroup_queue: child not found in parent queue");
}

/* Verify that TASK is in the team's task queue.  */

static void
verify_task_queue (struct gomp_task *task, struct gomp_team *team)
{
  struct gomp_task *t = team->task_queue;
  if (team)
    while (1)
      {
	if (t == task)
	  return;
	t = t->next_queue;
	if (t == team->task_queue)
	  break;
      }
  gomp_fatal ("verify_team_queue: child not in team");
}
#endif

static inline bool
gomp_task_run_pre (struct gomp_task *child_task, struct gomp_task *parent,
		   struct gomp_team *team)
{
#if _LIBGOMP_CHECKING
  verify_children_queue (child_task, parent);
  verify_taskgroup_queue (child_task);
  verify_task_queue (child_task, team);
#endif

  if (parent)
    {
      /* Adjust children such that it will point to a next child,
	 while the current one is scheduled to be executed.  This way,
	 GOMP_taskwait (and others) can schedule a next task while
	 waiting.

	 Do not remove it entirely from the circular list, as it is
	 still a child, though not one we should consider first (say
	 by GOMP_taskwait).  */
      if (parent->children == child_task)
	parent->children = child_task->next_child;
      /* TIED tasks cannot come before WAITING tasks.  If we're about
	 to make this task TIED, rewire things appropriately.
	 However, a TIED task at the end is perfectly fine.  */
      else if (child_task->next_child->kind == GOMP_TASK_WAITING
	       && child_task->next_child != parent->children)
	{
	  /* Remove from the list.  */
	  child_task->prev_child->next_child = child_task->next_child;
	  child_task->next_child->prev_child = child_task->prev_child;
	  /* Rewire at the end of its siblings.  */
	  child_task->next_child = parent->children;
	  child_task->prev_child = parent->children->prev_child;
	  parent->children->prev_child->next_child = child_task;
	  parent->children->prev_child = child_task;
	}

      /* If the current task (child_task) is at the top of the
	 parent's last_parent_depends_on, it's about to be removed
	 from it.  Adjust last_parent_depends_on appropriately.  */
      if (__builtin_expect (child_task->parent_depends_on, 0)
	  && parent->taskwait->last_parent_depends_on == child_task)
	{
	  /* The last_parent_depends_on list was built with all
	     parent_depends_on entries linked to the prev_child.  Grab
	     the next last_parent_depends_on head from this prev_child if
	     available...  */
	  if (child_task->prev_child->kind == GOMP_TASK_WAITING
	      && child_task->prev_child->parent_depends_on)
	    parent->taskwait->last_parent_depends_on = child_task->prev_child;
	  else
	    {
	      /* ...otherwise, there are no more parent_depends_on
		 entries waiting to run.  In which case, clear the
		 list.  */
	      parent->taskwait->last_parent_depends_on = NULL;
	    }
	}
    }

  /* Adjust taskgroup to point to the next taskgroup.  See note above
     regarding adjustment of children as to why the child_task is not
     removed entirely from the circular list.  */
  struct gomp_taskgroup *taskgroup = child_task->taskgroup;
  if (taskgroup)
    {
      if (taskgroup->children == child_task)
	taskgroup->children = child_task->next_taskgroup;
      /* TIED tasks cannot come before WAITING tasks.  If we're about
	 to make this task TIED, rewire things appropriately.
	 However, a TIED task at the end is perfectly fine.  */
      else if (child_task->next_taskgroup->kind == GOMP_TASK_WAITING
	       && child_task->next_taskgroup != taskgroup->children)
	{
	  /* Remove from the list.  */
	  child_task->prev_taskgroup->next_taskgroup
	    = child_task->next_taskgroup;
	  child_task->next_taskgroup->prev_taskgroup
	    = child_task->prev_taskgroup;
	  /* Rewire at the end of its taskgroup.  */
	  child_task->next_taskgroup = taskgroup->children;
	  child_task->prev_taskgroup = taskgroup->children->prev_taskgroup;
	  taskgroup->children->prev_taskgroup->next_taskgroup = child_task;
	  taskgroup->children->prev_taskgroup = child_task;
	}
    }

  /* Remove child_task from the task_queue.  */
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

/* After CHILD_TASK has been run, adjust the various task queues to
   give higher priority to the tasks that depend on CHILD_TASK.

   TEAM is the team to which CHILD_TASK belongs to.  */

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
	      /* If parent is in gomp_task_maybe_wait_for_dependencies
		 and it doesn't need to wait for this task, put it after
		 all ready to run tasks it needs to wait for.  */
	      if (parent->taskwait && parent->taskwait->last_parent_depends_on
		  && !task->parent_depends_on)
		{
		  /* Put depender in last_parent_depends_on.  */
		  struct gomp_task *last_parent_depends_on
		    = parent->taskwait->last_parent_depends_on;
		  task->next_child = last_parent_depends_on->next_child;
		  task->prev_child = last_parent_depends_on;
		}
	      else
		{
		  /* Make depender a sibling of child_task, and place
		     it at the top of said sibling list.  */
		  task->next_child = parent->children;
		  task->prev_child = parent->children->prev_child;
		  parent->children = task;
		}
	      task->next_child->prev_child = task;
	      task->prev_child->next_child = task;
	    }
	  else
	    {
	      /* Make depender a sibling of child_task.  */
	      task->next_child = task;
	      task->prev_child = task;
	      parent->children = task;
	    }
	  if (parent->taskwait)
	    {
	      if (parent->taskwait->in_taskwait)
		{
		  parent->taskwait->in_taskwait = false;
		  gomp_sem_post (&parent->taskwait->taskwait_sem);
		}
	      else if (parent->taskwait->in_depend_wait)
		{
		  parent->taskwait->in_depend_wait = false;
		  gomp_sem_post (&parent->taskwait->taskwait_sem);
		}
	      if (parent->taskwait->last_parent_depends_on == NULL
		  && task->parent_depends_on)
		parent->taskwait->last_parent_depends_on = task;
	    }
	}
      /* If depender is in a taskgroup, put it at the TOP of its
	 taskgroup.  */
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
      /* Put depender of child_task at the END of the team's
	 task_queue.  */
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

/* Remove CHILD_TASK from its parent.  */

static inline void
gomp_task_run_post_remove_parent (struct gomp_task *child_task)
{
  struct gomp_task *parent = child_task->parent;
  if (parent == NULL)
    return;

  /* If this was the last task the parent was depending on,
     synchronize with gomp_task_maybe_wait_for_dependencies so it can
     clean up and return.  */
  if (__builtin_expect (child_task->parent_depends_on, 0)
      && --parent->taskwait->n_depend == 0
      && parent->taskwait->in_depend_wait)
    {
      parent->taskwait->in_depend_wait = false;
      gomp_sem_post (&parent->taskwait->taskwait_sem);
    }

  /* Remove CHILD_TASK from its sibling list.  */
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
      if (parent->taskwait && parent->taskwait->in_taskwait)
	{
	  parent->taskwait->in_taskwait = false;
	  gomp_sem_post (&parent->taskwait->taskwait_sem);
	}
    }
}

/* Remove CHILD_TASK from its taskgroup.  */

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

/* Called when encountering a taskwait directive.

   Wait for all children of the current task.  */

void
GOMP_taskwait (void)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  struct gomp_task *task = thr->task;
  struct gomp_task *child_task = NULL;
  struct gomp_task *to_free = NULL;
  struct gomp_taskwait taskwait;
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

  memset (&taskwait, 0, sizeof (taskwait));
  gomp_mutex_lock (&team->task_lock);
  while (1)
    {
      bool cancelled = false;
      if (task->children == NULL)
	{
	  bool destroy_taskwait = task->taskwait != NULL;
	  task->taskwait = NULL;
	  gomp_mutex_unlock (&team->task_lock);
	  if (to_free)
	    {
	      gomp_finish_task (to_free);
	      free (to_free);
	    }
	  if (destroy_taskwait)
	    gomp_sem_destroy (&taskwait.taskwait_sem);
	  return;
	}
      if (task->children->kind == GOMP_TASK_WAITING)
	{
	  child_task = task->children;
	  cancelled
	    = gomp_task_run_pre (child_task, task, team);
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
	  /* All tasks we are waiting for are already running
	     in other threads.  Wait for them.  */
	  if (task->taskwait == NULL)
	    {
	      taskwait.in_depend_wait = false;
	      gomp_sem_init (&taskwait.taskwait_sem, 0);
	      task->taskwait = &taskwait;
	    }
	  taskwait.in_taskwait = true;
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
	gomp_sem_wait (&taskwait.taskwait_sem);
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	 finish_cancelled:;
	  size_t new_tasks
	    = gomp_task_run_post_handle_depend (child_task, team);

	  /* Remove child_task from children list, and set up the next
	     sibling to be run.  */
	  child_task->prev_child->next_child = child_task->next_child;
	  child_task->next_child->prev_child = child_task->prev_child;
	  if (task->children == child_task)
	    {
	      if (child_task->next_child != child_task)
		task->children = child_task->next_child;
	      else
		task->children = NULL;
	    }
	  /* Orphan all the children of CHILD_TASK.  */
	  gomp_clear_parent (child_task->children);

	  /* Remove CHILD_TASK from its taskgroup.  */
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

/* This is like GOMP_taskwait, but we only wait for tasks that the
   upcoming task depends on.

   DEPEND is as in GOMP_task.  */

void
gomp_task_maybe_wait_for_dependencies (void **depend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task *task = thr->task;
  struct gomp_team *team = thr->ts.team;
  struct gomp_task_depend_entry elem, *ent = NULL;
  struct gomp_taskwait taskwait;
  struct gomp_task *last_parent_depends_on = NULL;
  size_t ndepend = (uintptr_t) depend[0];
  size_t nout = (uintptr_t) depend[1];
  size_t i;
  size_t num_awaited = 0;
  struct gomp_task *child_task = NULL;
  struct gomp_task *to_free = NULL;
  int do_wake = 0;

  gomp_mutex_lock (&team->task_lock);
  for (i = 0; i < ndepend; i++)
    {
      elem.addr = depend[i + 2];
      ent = htab_find (task->depend_hash, &elem);
      for (; ent; ent = ent->next)
	if (i >= nout && ent->is_in)
	  continue;
	else
	  {
	    struct gomp_task *tsk = ent->task;
	    if (!tsk->parent_depends_on)
	      {
		tsk->parent_depends_on = true;
		++num_awaited;
		/* If a task we need to wait for is not already
		   running and is ready to be scheduled, move it to
		   front, so that we run it as soon as possible.

		   We rearrange the children queue such that all
		   parent_depends_on tasks are first, and
		   last_parent_depends_on points to the last such task
		   we rearranged.  For example, given the following
		   children where PD[123] are the parent_depends_on
		   tasks:

			task->children
			|
			V
			C1 -> C2 -> C3 -> PD1 -> PD2 -> PD3 -> C4

		   We rearrange such that:

			task->children
			|	       +--- last_parent_depends_on
			|	       |
			V	       V
			PD1 -> PD2 -> PD3 -> C1 -> C2 -> C3 -> C4
		*/

		if (tsk->num_dependees == 0 && tsk->kind == GOMP_TASK_WAITING)
		  {
		    if (last_parent_depends_on)
		      {
			tsk->prev_child->next_child = tsk->next_child;
			tsk->next_child->prev_child = tsk->prev_child;
			tsk->prev_child = last_parent_depends_on;
			tsk->next_child = last_parent_depends_on->next_child;
			tsk->prev_child->next_child = tsk;
			tsk->next_child->prev_child = tsk;
		      }
		    else if (tsk != task->children)
		      {
			tsk->prev_child->next_child = tsk->next_child;
			tsk->next_child->prev_child = tsk->prev_child;
			tsk->prev_child = task->children->prev_child;
			tsk->next_child = task->children;
			task->children = tsk;
			tsk->prev_child->next_child = tsk;
			tsk->next_child->prev_child = tsk;
		      }
		    last_parent_depends_on = tsk;
		  }
	      }
	  }
    }
  if (num_awaited == 0)
    {
      gomp_mutex_unlock (&team->task_lock);
      return;
    }

  memset (&taskwait, 0, sizeof (taskwait));
  taskwait.n_depend = num_awaited;
  taskwait.last_parent_depends_on = last_parent_depends_on;
  gomp_sem_init (&taskwait.taskwait_sem, 0);
  task->taskwait = &taskwait;

  while (1)
    {
      bool cancelled = false;
      if (taskwait.n_depend == 0)
	{
	  task->taskwait = NULL;
	  gomp_mutex_unlock (&team->task_lock);
	  if (to_free)
	    {
	      gomp_finish_task (to_free);
	      free (to_free);
	    }
	  gomp_sem_destroy (&taskwait.taskwait_sem);
	  return;
	}
      if (task->children->kind == GOMP_TASK_WAITING)
	{
	  child_task = task->children;
	  cancelled
	    = gomp_task_run_pre (child_task, task, team);
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
	taskwait.in_depend_wait = true;
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
	gomp_sem_wait (&taskwait.taskwait_sem);
      gomp_mutex_lock (&team->task_lock);
      if (child_task)
	{
	 finish_cancelled:;
	  size_t new_tasks
	    = gomp_task_run_post_handle_depend (child_task, team);
	  if (child_task->parent_depends_on)
	    --taskwait.n_depend;

	  /* Remove child_task from sibling list.  */
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
     GOMP_TASK_UNDEFERRED tasks and thus all children tasks of
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
	    {
	      if (task->children == NULL)
		goto do_wait;
	      child_task = task->children;
            }
          else
	    {
	      gomp_mutex_unlock (&team->task_lock);
	      if (to_free)
		{
		  gomp_finish_task (to_free);
		  free (to_free);
		}
	      goto finish;
	    }
	}
      else
	child_task = taskgroup->children;
      if (child_task->kind == GOMP_TASK_WAITING)
	{
	  cancelled
	    = gomp_task_run_pre (child_task, child_task->parent, team);
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
	  child_task = NULL;
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
	  gomp_task_run_post_remove_parent (child_task);
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
