/* Copyright (C) 2007-2016 Free Software Foundation, Inc.
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
  /* It would seem that using memset here would be a win, but it turns
     out that partially filling gomp_task allows us to keep the
     overhead of task creation low.  In the nqueens-1.c test, for a
     sufficiently large N, we drop the overhead from 5-6% to 1%.

     Note, the nqueens-1.c test in serial mode is a good test to
     benchmark the overhead of creating tasks as there are millions of
     tiny tasks created that all run undeferred.  */
  task->parent = parent_task;
  task->icv = *prev_icv;
  task->kind = GOMP_TASK_IMPLICIT;
  task->taskwait = NULL;
  task->in_tied_task = false;
  task->final_task = false;
  task->copy_ctors_done = false;
  task->parent_depends_on = false;
  priority_queue_init (&task->children_queue);
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

/* Clear the parent field of every task in LIST.  */

static inline void
gomp_clear_parent_in_list (struct priority_list *list)
{
  struct priority_node *p = list->tasks;
  if (p)
    do
      {
	priority_node_to_task (PQ_CHILDREN, p)->parent = NULL;
	p = p->next;
      }
    while (p != list->tasks);
}

/* Splay tree version of gomp_clear_parent_in_list.

   Clear the parent field of every task in NODE within SP, and free
   the node when done.  */

static void
gomp_clear_parent_in_tree (prio_splay_tree sp, prio_splay_tree_node node)
{
  if (!node)
    return;
  prio_splay_tree_node left = node->left, right = node->right;
  gomp_clear_parent_in_list (&node->key.l);
#if _LIBGOMP_CHECKING_
  memset (node, 0xaf, sizeof (*node));
#endif
  /* No need to remove the node from the tree.  We're nuking
     everything, so just free the nodes and our caller can clear the
     entire splay tree.  */
  free (node);
  gomp_clear_parent_in_tree (sp, left);
  gomp_clear_parent_in_tree (sp, right);
}

/* Clear the parent field of every task in Q and remove every task
   from Q.  */

static inline void
gomp_clear_parent (struct priority_queue *q)
{
  if (priority_queue_multi_p (q))
    {
      gomp_clear_parent_in_tree (&q->t, q->t.root);
      /* All the nodes have been cleared in gomp_clear_parent_in_tree.
	 No need to remove anything.  We can just nuke everything.  */
      q->t.root = NULL;
    }
  else
    gomp_clear_parent_in_list (&q->l);
}

/* Helper function for GOMP_task and gomp_create_target_task.

   For a TASK with in/out dependencies, fill in the various dependency
   queues.  PARENT is the parent of said task.  DEPEND is as in
   GOMP_task.  */

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
  else if (priority > gomp_max_task_priority_var)
    priority = gomp_max_task_priority_var;

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
      task.priority = priority;
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
      if (!priority_queue_empty_p (&task.children_queue, MEMMODEL_RELAXED))
	{
	  gomp_mutex_lock (&team->task_lock);
	  gomp_clear_parent (&task.children_queue);
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
      task->priority = priority;
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
	      /* Tasks that depend on other tasks are not put into the
		 various waiting queues, so we are done for now.  Said
		 tasks are instead put into the queues via
		 gomp_task_run_post_handle_dependers() after their
		 dependencies have been satisfied.  After which, they
		 can be picked up by the various scheduling
		 points.  */
	      gomp_mutex_unlock (&team->task_lock);
	      return;
	    }
	}

      priority_queue_insert (PQ_CHILDREN, &parent->children_queue,
			     task, priority,
			     PRIORITY_INSERT_BEGIN,
			     /*adjust_parent_depends_on=*/false,
			     task->parent_depends_on);
      if (taskgroup)
	priority_queue_insert (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
			       task, priority,
			       PRIORITY_INSERT_BEGIN,
			       /*adjust_parent_depends_on=*/false,
			       task->parent_depends_on);

      priority_queue_insert (PQ_TEAM, &team->task_queue,
			     task, priority,
			     PRIORITY_INSERT_END,
			     /*adjust_parent_depends_on=*/false,
			     task->parent_depends_on);

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

static void inline
priority_queue_move_task_first (enum priority_queue_type type,
				struct priority_queue *head,
				struct gomp_task *task)
{
#if _LIBGOMP_CHECKING_
  if (!priority_queue_task_in_queue_p (type, head, task))
    gomp_fatal ("Attempt to move first missing task %p", task);
#endif
  struct priority_list *list;
  if (priority_queue_multi_p (head))
    {
      list = priority_queue_lookup_priority (head, task->priority);
#if _LIBGOMP_CHECKING_
      if (!list)
	gomp_fatal ("Unable to find priority %d", task->priority);
#endif
    }
  else
    list = &head->l;
  priority_list_remove (list, task_to_priority_node (type, task), 0);
  priority_list_insert (type, list, task, task->priority,
			PRIORITY_INSERT_BEGIN, type == PQ_CHILDREN,
			task->parent_depends_on);
}

/* Actual body of GOMP_PLUGIN_target_task_completion that is executed
   with team->task_lock held, or is executed in the thread that called
   gomp_target_task_fn if GOMP_PLUGIN_target_task_completion has been
   run before it acquires team->task_lock.  */

static void
gomp_target_task_completion (struct gomp_team *team, struct gomp_task *task)
{
  struct gomp_task *parent = task->parent;
  if (parent)
    priority_queue_move_task_first (PQ_CHILDREN, &parent->children_queue,
				    task);

  struct gomp_taskgroup *taskgroup = task->taskgroup;
  if (taskgroup)
    priority_queue_move_task_first (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
				    task);

  priority_queue_insert (PQ_TEAM, &team->task_queue, task, task->priority,
			 PRIORITY_INSERT_BEGIN, false,
			 task->parent_depends_on);
  task->kind = GOMP_TASK_WAITING;
  if (parent && parent->taskwait)
    {
      if (parent->taskwait->in_taskwait)
	{
	  /* One more task has had its dependencies met.
	     Inform any waiters.  */
	  parent->taskwait->in_taskwait = false;
	  gomp_sem_post (&parent->taskwait->taskwait_sem);
	}
      else if (parent->taskwait->in_depend_wait)
	{
	  /* One more task has had its dependencies met.
	     Inform any waiters.  */
	  parent->taskwait->in_depend_wait = false;
	  gomp_sem_post (&parent->taskwait->taskwait_sem);
	}
    }
  if (taskgroup && taskgroup->in_taskgroup_wait)
    {
      /* One more task has had its dependencies met.
	 Inform any waiters.  */
      taskgroup->in_taskgroup_wait = false;
      gomp_sem_post (&taskgroup->taskgroup_sem);
    }

  ++team->task_queued_count;
  gomp_team_barrier_set_task_pending (&team->barrier);
  /* I'm afraid this can't be done after releasing team->task_lock,
     as gomp_target_task_completion is run from unrelated thread and
     therefore in between gomp_mutex_unlock and gomp_team_barrier_wake
     the team could be gone already.  */
  if (team->nthreads > team->task_running_count)
    gomp_team_barrier_wake (&team->barrier, 1);
}

/* Signal that a target task TTASK has completed the asynchronously
   running phase and should be requeued as a task to handle the
   variable unmapping.  */

void
GOMP_PLUGIN_target_task_completion (void *data)
{
  struct gomp_target_task *ttask = (struct gomp_target_task *) data;
  struct gomp_task *task = ttask->task;
  struct gomp_team *team = ttask->team;

  gomp_mutex_lock (&team->task_lock);
  if (ttask->state == GOMP_TARGET_TASK_READY_TO_RUN)
    {
      ttask->state = GOMP_TARGET_TASK_FINISHED;
      gomp_mutex_unlock (&team->task_lock);
      return;
    }
  ttask->state = GOMP_TARGET_TASK_FINISHED;
  gomp_target_task_completion (team, task);
  gomp_mutex_unlock (&team->task_lock);
}

static void gomp_task_run_post_handle_depend_hash (struct gomp_task *);

/* Called for nowait target tasks.  */

bool
gomp_create_target_task (struct gomp_device_descr *devicep,
			 void (*fn) (void *), size_t mapnum, void **hostaddrs,
			 size_t *sizes, unsigned short *kinds,
			 unsigned int flags, void **depend, void **args,
			 enum gomp_target_task_state state)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return true;

  struct gomp_target_task *ttask;
  struct gomp_task *task;
  struct gomp_task *parent = thr->task;
  struct gomp_taskgroup *taskgroup = parent->taskgroup;
  bool do_wake;
  size_t depend_size = 0;
  uintptr_t depend_cnt = 0;
  size_t tgt_align = 0, tgt_size = 0;

  if (depend != NULL)
    {
      depend_cnt = (uintptr_t) depend[0];
      depend_size = depend_cnt * sizeof (struct gomp_task_depend_entry);
    }
  if (fn)
    {
      /* GOMP_MAP_FIRSTPRIVATE need to be copied first, as they are
	 firstprivate on the target task.  */
      size_t i;
      for (i = 0; i < mapnum; i++)
	if ((kinds[i] & 0xff) == GOMP_MAP_FIRSTPRIVATE)
	  {
	    size_t align = (size_t) 1 << (kinds[i] >> 8);
	    if (tgt_align < align)
	      tgt_align = align;
	    tgt_size = (tgt_size + align - 1) & ~(align - 1);
	    tgt_size += sizes[i];
	  }
      if (tgt_align)
	tgt_size += tgt_align - 1;
      else
	tgt_size = 0;
    }

  task = gomp_malloc (sizeof (*task) + depend_size
		      + sizeof (*ttask)
		      + mapnum * (sizeof (void *) + sizeof (size_t)
				  + sizeof (unsigned short))
		      + tgt_size);
  gomp_init_task (task, parent, gomp_icv (false));
  task->priority = 0;
  task->kind = GOMP_TASK_WAITING;
  task->in_tied_task = parent->in_tied_task;
  task->taskgroup = taskgroup;
  ttask = (struct gomp_target_task *) &task->depend[depend_cnt];
  ttask->devicep = devicep;
  ttask->fn = fn;
  ttask->mapnum = mapnum;
  ttask->args = args;
  memcpy (ttask->hostaddrs, hostaddrs, mapnum * sizeof (void *));
  ttask->sizes = (size_t *) &ttask->hostaddrs[mapnum];
  memcpy (ttask->sizes, sizes, mapnum * sizeof (size_t));
  ttask->kinds = (unsigned short *) &ttask->sizes[mapnum];
  memcpy (ttask->kinds, kinds, mapnum * sizeof (unsigned short));
  if (tgt_align)
    {
      char *tgt = (char *) &ttask->kinds[mapnum];
      size_t i;
      uintptr_t al = (uintptr_t) tgt & (tgt_align - 1);
      if (al)
	tgt += tgt_align - al;
      tgt_size = 0;
      for (i = 0; i < mapnum; i++)
	if ((kinds[i] & 0xff) == GOMP_MAP_FIRSTPRIVATE)
	  {
	    size_t align = (size_t) 1 << (kinds[i] >> 8);
	    tgt_size = (tgt_size + align - 1) & ~(align - 1);
	    memcpy (tgt + tgt_size, hostaddrs[i], sizes[i]);
	    ttask->hostaddrs[i] = tgt + tgt_size;
	    tgt_size = tgt_size + sizes[i];
	  }
    }
  ttask->flags = flags;
  ttask->state = state;
  ttask->task = task;
  ttask->team = team;
  task->fn = NULL;
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
      return true;
    }
  if (depend_size)
    {
      gomp_task_handle_depend (task, parent, depend);
      if (task->num_dependees)
	{
	  if (taskgroup)
	    taskgroup->num_children++;
	  gomp_mutex_unlock (&team->task_lock);
	  return true;
	}
    }
  if (state == GOMP_TARGET_TASK_DATA)
    {
      gomp_task_run_post_handle_depend_hash (task);
      gomp_mutex_unlock (&team->task_lock);
      gomp_finish_task (task);
      free (task);
      return false;
    }
  if (taskgroup)
    taskgroup->num_children++;
  /* For async offloading, if we don't need to wait for dependencies,
     run the gomp_target_task_fn right away, essentially schedule the
     mapping part of the task in the current thread.  */
  if (devicep != NULL
      && (devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400))
    {
      priority_queue_insert (PQ_CHILDREN, &parent->children_queue, task, 0,
			     PRIORITY_INSERT_END,
			     /*adjust_parent_depends_on=*/false,
			     task->parent_depends_on);
      if (taskgroup)
	priority_queue_insert (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
			       task, 0, PRIORITY_INSERT_END,
			       /*adjust_parent_depends_on=*/false,
			       task->parent_depends_on);
      task->pnode[PQ_TEAM].next = NULL;
      task->pnode[PQ_TEAM].prev = NULL;
      task->kind = GOMP_TASK_TIED;
      ++team->task_count;
      gomp_mutex_unlock (&team->task_lock);

      thr->task = task;
      gomp_target_task_fn (task->fn_data);
      thr->task = parent;

      gomp_mutex_lock (&team->task_lock);
      task->kind = GOMP_TASK_ASYNC_RUNNING;
      /* If GOMP_PLUGIN_target_task_completion has run already
	 in between gomp_target_task_fn and the mutex lock,
	 perform the requeuing here.  */
      if (ttask->state == GOMP_TARGET_TASK_FINISHED)
	gomp_target_task_completion (team, task);
      else
	ttask->state = GOMP_TARGET_TASK_RUNNING;
      gomp_mutex_unlock (&team->task_lock);
      return true;
    }
  priority_queue_insert (PQ_CHILDREN, &parent->children_queue, task, 0,
			 PRIORITY_INSERT_BEGIN,
			 /*adjust_parent_depends_on=*/false,
			 task->parent_depends_on);
  if (taskgroup)
    priority_queue_insert (PQ_TASKGROUP, &taskgroup->taskgroup_queue, task, 0,
			   PRIORITY_INSERT_BEGIN,
			   /*adjust_parent_depends_on=*/false,
			   task->parent_depends_on);
  priority_queue_insert (PQ_TEAM, &team->task_queue, task, 0,
			 PRIORITY_INSERT_END,
			 /*adjust_parent_depends_on=*/false,
			 task->parent_depends_on);
  ++team->task_count;
  ++team->task_queued_count;
  gomp_team_barrier_set_task_pending (&team->barrier);
  do_wake = team->task_running_count + !parent->in_tied_task
	    < team->nthreads;
  gomp_mutex_unlock (&team->task_lock);
  if (do_wake)
    gomp_team_barrier_wake (&team->barrier, 1);
  return true;
}

/* Given a parent_depends_on task in LIST, move it to the front of its
   priority so it is run as soon as possible.

   Care is taken to update the list's LAST_PARENT_DEPENDS_ON field.

   We rearrange the queue such that all parent_depends_on tasks are
   first, and last_parent_depends_on points to the last such task we
   rearranged.  For example, given the following tasks in a queue
   where PD[123] are the parent_depends_on tasks:

	task->children
	|
	V
	C1 -> C2 -> C3 -> PD1 -> PD2 -> PD3 -> C4

	We rearrange such that:

	task->children
	|	       +--- last_parent_depends_on
	|	       |
	V	       V
	PD1 -> PD2 -> PD3 -> C1 -> C2 -> C3 -> C4.  */

static void inline
priority_list_upgrade_task (struct priority_list *list,
			    struct priority_node *node)
{
  struct priority_node *last_parent_depends_on
    = list->last_parent_depends_on;
  if (last_parent_depends_on)
    {
      node->prev->next = node->next;
      node->next->prev = node->prev;
      node->prev = last_parent_depends_on;
      node->next = last_parent_depends_on->next;
      node->prev->next = node;
      node->next->prev = node;
    }
  else if (node != list->tasks)
    {
      node->prev->next = node->next;
      node->next->prev = node->prev;
      node->prev = list->tasks->prev;
      node->next = list->tasks;
      list->tasks = node;
      node->prev->next = node;
      node->next->prev = node;
    }
  list->last_parent_depends_on = node;
}

/* Given a parent_depends_on TASK in its parent's children_queue, move
   it to the front of its priority so it is run as soon as possible.

   PARENT is passed as an optimization.

   (This function could be defined in priority_queue.c, but we want it
   inlined, and putting it in priority_queue.h is not an option, given
   that gomp_task has not been properly defined at that point).  */

static void inline
priority_queue_upgrade_task (struct gomp_task *task,
			     struct gomp_task *parent)
{
  struct priority_queue *head = &parent->children_queue;
  struct priority_node *node = &task->pnode[PQ_CHILDREN];
#if _LIBGOMP_CHECKING_
  if (!task->parent_depends_on)
    gomp_fatal ("priority_queue_upgrade_task: task must be a "
		"parent_depends_on task");
  if (!priority_queue_task_in_queue_p (PQ_CHILDREN, head, task))
    gomp_fatal ("priority_queue_upgrade_task: cannot find task=%p", task);
#endif
  if (priority_queue_multi_p (head))
    {
      struct priority_list *list
	= priority_queue_lookup_priority (head, task->priority);
      priority_list_upgrade_task (list, node);
    }
  else
    priority_list_upgrade_task (&head->l, node);
}

/* Given a CHILD_TASK in LIST that is about to be executed, move it out of
   the way in LIST so that other tasks can be considered for
   execution.  LIST contains tasks of type TYPE.

   Care is taken to update the queue's LAST_PARENT_DEPENDS_ON field
   if applicable.  */

static void inline
priority_list_downgrade_task (enum priority_queue_type type,
			      struct priority_list *list,
			      struct gomp_task *child_task)
{
  struct priority_node *node = task_to_priority_node (type, child_task);
  if (list->tasks == node)
    list->tasks = node->next;
  else if (node->next != list->tasks)
    {
      /* The task in NODE is about to become TIED and TIED tasks
	 cannot come before WAITING tasks.  If we're about to
	 leave the queue in such an indeterminate state, rewire
	 things appropriately.  However, a TIED task at the end is
	 perfectly fine.  */
      struct gomp_task *next_task = priority_node_to_task (type, node->next);
      if (next_task->kind == GOMP_TASK_WAITING)
	{
	  /* Remove from list.  */
	  node->prev->next = node->next;
	  node->next->prev = node->prev;
	  /* Rewire at the end.  */
	  node->next = list->tasks;
	  node->prev = list->tasks->prev;
	  list->tasks->prev->next = node;
	  list->tasks->prev = node;
	}
    }

  /* If the current task is the last_parent_depends_on for its
     priority, adjust last_parent_depends_on appropriately.  */
  if (__builtin_expect (child_task->parent_depends_on, 0)
      && list->last_parent_depends_on == node)
    {
      struct gomp_task *prev_child = priority_node_to_task (type, node->prev);
      if (node->prev != node
	  && prev_child->kind == GOMP_TASK_WAITING
	  && prev_child->parent_depends_on)
	list->last_parent_depends_on = node->prev;
      else
	{
	  /* There are no more parent_depends_on entries waiting
	     to run, clear the list.  */
	  list->last_parent_depends_on = NULL;
	}
    }
}

/* Given a TASK in HEAD that is about to be executed, move it out of
   the way so that other tasks can be considered for execution.  HEAD
   contains tasks of type TYPE.

   Care is taken to update the queue's LAST_PARENT_DEPENDS_ON field
   if applicable.

   (This function could be defined in priority_queue.c, but we want it
   inlined, and putting it in priority_queue.h is not an option, given
   that gomp_task has not been properly defined at that point).  */

static void inline
priority_queue_downgrade_task (enum priority_queue_type type,
			       struct priority_queue *head,
			       struct gomp_task *task)
{
#if _LIBGOMP_CHECKING_
  if (!priority_queue_task_in_queue_p (type, head, task))
    gomp_fatal ("Attempt to downgrade missing task %p", task);
#endif
  if (priority_queue_multi_p (head))
    {
      struct priority_list *list
	= priority_queue_lookup_priority (head, task->priority);
      priority_list_downgrade_task (type, list, task);
    }
  else
    priority_list_downgrade_task (type, &head->l, task);
}

/* Setup CHILD_TASK to execute.  This is done by setting the task to
   TIED, and updating all relevant queues so that CHILD_TASK is no
   longer chosen for scheduling.  Also, remove CHILD_TASK from the
   overall team task queue entirely.

   Return TRUE if task or its containing taskgroup has been
   cancelled.  */

static inline bool
gomp_task_run_pre (struct gomp_task *child_task, struct gomp_task *parent,
		   struct gomp_team *team)
{
#if _LIBGOMP_CHECKING_
  if (child_task->parent)
    priority_queue_verify (PQ_CHILDREN,
			   &child_task->parent->children_queue, true);
  if (child_task->taskgroup)
    priority_queue_verify (PQ_TASKGROUP,
			   &child_task->taskgroup->taskgroup_queue, false);
  priority_queue_verify (PQ_TEAM, &team->task_queue, false);
#endif

  /* Task is about to go tied, move it out of the way.  */
  if (parent)
    priority_queue_downgrade_task (PQ_CHILDREN, &parent->children_queue,
				   child_task);

  /* Task is about to go tied, move it out of the way.  */
  struct gomp_taskgroup *taskgroup = child_task->taskgroup;
  if (taskgroup)
    priority_queue_downgrade_task (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
				   child_task);

  priority_queue_remove (PQ_TEAM, &team->task_queue, child_task,
			 MEMMODEL_RELAXED);
  child_task->pnode[PQ_TEAM].next = NULL;
  child_task->pnode[PQ_TEAM].prev = NULL;
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

/* After a CHILD_TASK has been run, adjust the dependency queue for
   each task that depends on CHILD_TASK, to record the fact that there
   is one less dependency to worry about.  If a task that depended on
   CHILD_TASK now has no dependencies, place it in the various queues
   so it gets scheduled to run.

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

      /* CHILD_TASK satisfies a dependency for TASK.  Keep track of
	 TASK's remaining dependencies.  Once TASK has no other
	 depenencies, put it into the various queues so it will get
	 scheduled for execution.  */
      if (--task->num_dependees != 0)
	continue;

      struct gomp_taskgroup *taskgroup = task->taskgroup;
      if (parent)
	{
	  priority_queue_insert (PQ_CHILDREN, &parent->children_queue,
				 task, task->priority,
				 PRIORITY_INSERT_BEGIN,
				 /*adjust_parent_depends_on=*/true,
				 task->parent_depends_on);
	  if (parent->taskwait)
	    {
	      if (parent->taskwait->in_taskwait)
		{
		  /* One more task has had its dependencies met.
		     Inform any waiters.  */
		  parent->taskwait->in_taskwait = false;
		  gomp_sem_post (&parent->taskwait->taskwait_sem);
		}
	      else if (parent->taskwait->in_depend_wait)
		{
		  /* One more task has had its dependencies met.
		     Inform any waiters.  */
		  parent->taskwait->in_depend_wait = false;
		  gomp_sem_post (&parent->taskwait->taskwait_sem);
		}
	    }
	}
      if (taskgroup)
	{
	  priority_queue_insert (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
				 task, task->priority,
				 PRIORITY_INSERT_BEGIN,
				 /*adjust_parent_depends_on=*/false,
				 task->parent_depends_on);
	  if (taskgroup->in_taskgroup_wait)
	    {
	      /* One more task has had its dependencies met.
		 Inform any waiters.  */
	      taskgroup->in_taskgroup_wait = false;
	      gomp_sem_post (&taskgroup->taskgroup_sem);
	    }
	}
      priority_queue_insert (PQ_TEAM, &team->task_queue,
			     task, task->priority,
			     PRIORITY_INSERT_END,
			     /*adjust_parent_depends_on=*/false,
			     task->parent_depends_on);
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

  if (priority_queue_remove (PQ_CHILDREN, &parent->children_queue,
			     child_task, MEMMODEL_RELEASE)
      && parent->taskwait && parent->taskwait->in_taskwait)
    {
      parent->taskwait->in_taskwait = false;
      gomp_sem_post (&parent->taskwait->taskwait_sem);
    }
  child_task->pnode[PQ_CHILDREN].next = NULL;
  child_task->pnode[PQ_CHILDREN].prev = NULL;
}

/* Remove CHILD_TASK from its taskgroup.  */

static inline void
gomp_task_run_post_remove_taskgroup (struct gomp_task *child_task)
{
  struct gomp_taskgroup *taskgroup = child_task->taskgroup;
  if (taskgroup == NULL)
    return;
  bool empty = priority_queue_remove (PQ_TASKGROUP,
				      &taskgroup->taskgroup_queue,
				      child_task, MEMMODEL_RELAXED);
  child_task->pnode[PQ_TASKGROUP].next = NULL;
  child_task->pnode[PQ_TASKGROUP].prev = NULL;
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
  if (empty && taskgroup->in_taskgroup_wait)
    {
      taskgroup->in_taskgroup_wait = false;
      gomp_sem_post (&taskgroup->taskgroup_sem);
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
      if (!priority_queue_empty_p (&team->task_queue, MEMMODEL_RELAXED))
	{
	  bool ignored;
	  child_task
	    = priority_queue_next_task (PQ_TEAM, &team->task_queue,
					PQ_IGNORED, NULL,
					&ignored);
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
	  if (__builtin_expect (child_task->fn == NULL, 0))
	    {
	      if (gomp_target_task_fn (child_task->fn_data))
		{
		  thr->task = task;
		  gomp_mutex_lock (&team->task_lock);
		  child_task->kind = GOMP_TASK_ASYNC_RUNNING;
		  team->task_running_count--;
		  struct gomp_target_task *ttask
		    = (struct gomp_target_task *) child_task->fn_data;
		  /* If GOMP_PLUGIN_target_task_completion has run already
		     in between gomp_target_task_fn and the mutex lock,
		     perform the requeuing here.  */
		  if (ttask->state == GOMP_TARGET_TASK_FINISHED)
		    gomp_target_task_completion (team, child_task);
		  else
		    ttask->state = GOMP_TARGET_TASK_RUNNING;
		  child_task = NULL;
		  continue;
		}
	    }
	  else
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
	  gomp_clear_parent (&child_task->children_queue);
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
      || priority_queue_empty_p (&task->children_queue, MEMMODEL_ACQUIRE))
    return;

  memset (&taskwait, 0, sizeof (taskwait));
  bool child_q = false;
  gomp_mutex_lock (&team->task_lock);
  while (1)
    {
      bool cancelled = false;
      if (priority_queue_empty_p (&task->children_queue, MEMMODEL_RELAXED))
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
      struct gomp_task *next_task
	= priority_queue_next_task (PQ_CHILDREN, &task->children_queue,
				    PQ_TEAM, &team->task_queue, &child_q);
      if (next_task->kind == GOMP_TASK_WAITING)
	{
	  child_task = next_task;
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
	/* All tasks we are waiting for are either running in other
	   threads, or they are tasks that have not had their
	   dependencies met (so they're not even in the queue).  Wait
	   for them.  */
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
	  if (__builtin_expect (child_task->fn == NULL, 0))
	    {
	      if (gomp_target_task_fn (child_task->fn_data))
		{
		  thr->task = task;
		  gomp_mutex_lock (&team->task_lock);
		  child_task->kind = GOMP_TASK_ASYNC_RUNNING;
		  struct gomp_target_task *ttask
		    = (struct gomp_target_task *) child_task->fn_data;
		  /* If GOMP_PLUGIN_target_task_completion has run already
		     in between gomp_target_task_fn and the mutex lock,
		     perform the requeuing here.  */
		  if (ttask->state == GOMP_TARGET_TASK_FINISHED)
		    gomp_target_task_completion (team, child_task);
		  else
		    ttask->state = GOMP_TARGET_TASK_RUNNING;
		  child_task = NULL;
		  continue;
		}
	    }
	  else
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

	  if (child_q)
	    {
	      priority_queue_remove (PQ_CHILDREN, &task->children_queue,
				     child_task, MEMMODEL_RELAXED);
	      child_task->pnode[PQ_CHILDREN].next = NULL;
	      child_task->pnode[PQ_CHILDREN].prev = NULL;
	    }

	  gomp_clear_parent (&child_task->children_queue);

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

/* An undeferred task is about to run.  Wait for all tasks that this
   undeferred task depends on.

   This is done by first putting all known ready dependencies
   (dependencies that have their own dependencies met) at the top of
   the scheduling queues.  Then we iterate through these imminently
   ready tasks (and possibly other high priority tasks), and run them.
   If we run out of ready dependencies to execute, we either wait for
   the reamining dependencies to finish, or wait for them to get
   scheduled so we can run them.

   DEPEND is as in GOMP_task.  */

void
gomp_task_maybe_wait_for_dependencies (void **depend)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_task *task = thr->task;
  struct gomp_team *team = thr->ts.team;
  struct gomp_task_depend_entry elem, *ent = NULL;
  struct gomp_taskwait taskwait;
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
		/* If depenency TSK itself has no dependencies and is
		   ready to run, move it up front so that we run it as
		   soon as possible.  */
		if (tsk->num_dependees == 0 && tsk->kind == GOMP_TASK_WAITING)
		  priority_queue_upgrade_task (tsk, task);
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

      /* Theoretically when we have multiple priorities, we should
	 chose between the highest priority item in
	 task->children_queue and team->task_queue here, so we should
	 use priority_queue_next_task().  However, since we are
	 running an undeferred task, perhaps that makes all tasks it
	 depends on undeferred, thus a priority of INF?  This would
	 make it unnecessary to take anything into account here,
	 but the dependencies.

	 On the other hand, if we want to use priority_queue_next_task(),
	 care should be taken to only use priority_queue_remove()
	 below if the task was actually removed from the children
	 queue.  */
      bool ignored;
      struct gomp_task *next_task
	= priority_queue_next_task (PQ_CHILDREN, &task->children_queue,
				    PQ_IGNORED, NULL, &ignored);

      if (next_task->kind == GOMP_TASK_WAITING)
	{
	  child_task = next_task;
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
	/* All tasks we are waiting for are either running in other
	   threads, or they are tasks that have not had their
	   dependencies met (so they're not even in the queue).  Wait
	   for them.  */
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
	  if (__builtin_expect (child_task->fn == NULL, 0))
	    {
	      if (gomp_target_task_fn (child_task->fn_data))
		{
		  thr->task = task;
		  gomp_mutex_lock (&team->task_lock);
		  child_task->kind = GOMP_TASK_ASYNC_RUNNING;
		  struct gomp_target_task *ttask
		    = (struct gomp_target_task *) child_task->fn_data;
		  /* If GOMP_PLUGIN_target_task_completion has run already
		     in between gomp_target_task_fn and the mutex lock,
		     perform the requeuing here.  */
		  if (ttask->state == GOMP_TARGET_TASK_FINISHED)
		    gomp_target_task_completion (team, child_task);
		  else
		    ttask->state = GOMP_TARGET_TASK_RUNNING;
		  child_task = NULL;
		  continue;
		}
	    }
	  else
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

	  priority_queue_remove (PQ_CHILDREN, &task->children_queue,
				 child_task, MEMMODEL_RELAXED);
	  child_task->pnode[PQ_CHILDREN].next = NULL;
	  child_task->pnode[PQ_CHILDREN].prev = NULL;

	  gomp_clear_parent (&child_task->children_queue);
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
  priority_queue_init (&taskgroup->taskgroup_queue);
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
  if (__builtin_expect (taskgroup == NULL, 0)
      && thr->ts.level == 0)
    {
      /* This can happen if GOMP_taskgroup_start is called when
	 thr->ts.team == NULL, but inside of the taskgroup there
	 is #pragma omp target nowait that creates an implicit
	 team with a single thread.  In this case, we want to wait
	 for all outstanding tasks in this team.  */
      gomp_team_barrier_wait (&team->barrier);
      return;
    }

  /* The acquire barrier on load of taskgroup->num_children here
     synchronizes with the write of 0 in gomp_task_run_post_remove_taskgroup.
     It is not necessary that we synchronize with other non-0 writes at
     this point, but we must ensure that all writes to memory by a
     child thread task work function are seen before we exit from
     GOMP_taskgroup_end.  */
  if (__atomic_load_n (&taskgroup->num_children, MEMMODEL_ACQUIRE) == 0)
    goto finish;

  bool unused;
  gomp_mutex_lock (&team->task_lock);
  while (1)
    {
      bool cancelled = false;
      if (priority_queue_empty_p (&taskgroup->taskgroup_queue,
				  MEMMODEL_RELAXED))
	{
	  if (taskgroup->num_children)
	    {
	      if (priority_queue_empty_p (&task->children_queue,
					  MEMMODEL_RELAXED))
		goto do_wait;
	      child_task
		= priority_queue_next_task (PQ_CHILDREN, &task->children_queue,
					    PQ_TEAM, &team->task_queue,
					    &unused);
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
	child_task
	  = priority_queue_next_task (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
				      PQ_TEAM, &team->task_queue, &unused);
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
	/* All tasks we are waiting for are either running in other
	   threads, or they are tasks that have not had their
	   dependencies met (so they're not even in the queue).  Wait
	   for them.  */
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
	  if (__builtin_expect (child_task->fn == NULL, 0))
	    {
	      if (gomp_target_task_fn (child_task->fn_data))
		{
		  thr->task = task;
		  gomp_mutex_lock (&team->task_lock);
		  child_task->kind = GOMP_TASK_ASYNC_RUNNING;
		  struct gomp_target_task *ttask
		    = (struct gomp_target_task *) child_task->fn_data;
		  /* If GOMP_PLUGIN_target_task_completion has run already
		     in between gomp_target_task_fn and the mutex lock,
		     perform the requeuing here.  */
		  if (ttask->state == GOMP_TARGET_TASK_FINISHED)
		    gomp_target_task_completion (team, child_task);
		  else
		    ttask->state = GOMP_TARGET_TASK_RUNNING;
		  child_task = NULL;
		  continue;
		}
	    }
	  else
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
	  gomp_clear_parent (&child_task->children_queue);
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
