/* Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

/* This file handles the taskloop construct.  It is included twice, once
   for the long and once for unsigned long long variant.  */

/* Called when encountering an explicit task directive.  If IF_CLAUSE is
   false, then we must not delay in executing the task.  If UNTIED is true,
   then the task may be executed by any member of the team.  */

void
GOMP_taskloop (void (*fn) (void *), void *data, void (*cpyfn) (void *, void *),
	       long arg_size, long arg_align, unsigned flags,
	       unsigned long num_tasks, int priority,
	       TYPE start, TYPE end, TYPE step)
{
  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;

#ifdef HAVE_BROKEN_POSIX_SEMAPHORES
  /* If pthread_mutex_* is used for omp_*lock*, then each task must be
     tied to one thread all the time.  This means UNTIED tasks must be
     tied and if CPYFN is non-NULL IF(0) must be forced, as CPYFN
     might be running on different thread than FN.  */
  if (cpyfn)
    flags &= ~GOMP_TASK_FLAG_IF;
  flags &= ~GOMP_TASK_FLAG_UNTIED;
#endif

  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team && gomp_team_barrier_cancelled (&team->barrier))
    {
    early_return:
      if ((flags & (GOMP_TASK_FLAG_NOGROUP | GOMP_TASK_FLAG_REDUCTION))
	  == GOMP_TASK_FLAG_REDUCTION)
	{
	  struct gomp_data_head { TYPE t1, t2; uintptr_t *ptr; };
	  uintptr_t *ptr = ((struct gomp_data_head *) data)->ptr;
	  /* Tell callers GOMP_taskgroup_reduction_register has not been
	     called.  */
	  ptr[2] = 0;
	}
      return;
    }

#ifdef TYPE_is_long
  TYPE s = step;
  if (step > 0)
    {
      if (start >= end)
	goto early_return;
      s--;
    }
  else
    {
      if (start <= end)
	goto early_return;
      s++;
    }
  UTYPE n = (end - start + s) / step;
#else
  UTYPE n;
  if (flags & GOMP_TASK_FLAG_UP)
    {
      if (start >= end)
	goto early_return;
      n = (end - start + step - 1) / step;
    }
  else
    {
      if (start <= end)
	goto early_return;
      n = (start - end - step - 1) / -step;
    }
#endif

  TYPE task_step = step;
  TYPE nfirst_task_step = step;
  unsigned long nfirst = n;
  if (flags & GOMP_TASK_FLAG_GRAINSIZE)
    {
      unsigned long grainsize = num_tasks;
#ifdef TYPE_is_long
      num_tasks = n / grainsize;
#else
      UTYPE ndiv = n / grainsize;
      num_tasks = ndiv;
      if (num_tasks != ndiv)
	num_tasks = ~0UL;
#endif
      if ((flags & GOMP_TASK_FLAG_STRICT)
	  && num_tasks != ~0ULL)
	{
	  UTYPE mod = n % grainsize;
	  task_step = (TYPE) grainsize * step;
	  if (mod)
	    {
	      num_tasks++;
	      nfirst_task_step = (TYPE) mod * step;
	      if (num_tasks == 1)
		task_step = nfirst_task_step;
	      else
		nfirst = num_tasks - 2;
	    }
	}
      else if (num_tasks <= 1)
	{
	  num_tasks = 1;
	  task_step = end - start;
	}
      else if (num_tasks >= grainsize
#ifndef TYPE_is_long
	       && num_tasks != ~0UL
#endif
	      )
	{
	  UTYPE mul = num_tasks * grainsize;
	  task_step = (TYPE) grainsize * step;
	  if (mul != n)
	    {
	      nfirst_task_step = task_step;
	      task_step += step;
	      nfirst = n - mul - 1;
	    }
	}
      else
	{
	  UTYPE div = n / num_tasks;
	  UTYPE mod = n % num_tasks;
	  task_step = (TYPE) div * step;
	  if (mod)
	    {
	      nfirst_task_step = task_step;
	      task_step += step;
	      nfirst = mod - 1;
	    }
	}
    }
  else
    {
      if (num_tasks == 0)
	num_tasks = team ? team->nthreads : 1;
      if (num_tasks >= n)
	num_tasks = n;
      else
	{
	  UTYPE div = n / num_tasks;
	  UTYPE mod = n % num_tasks;
	  task_step = (TYPE) div * step;
	  if (mod)
	    {
	      nfirst_task_step = task_step;
	      task_step += step;
	      nfirst = mod - 1;
	    }
	}
    }

  if (flags & GOMP_TASK_FLAG_NOGROUP)
    {
      if (__builtin_expect (gomp_cancel_var, 0)
	  && thr->task
	  && thr->task->taskgroup)
	{
	  if (thr->task->taskgroup->cancelled)
	    return;
	  if (thr->task->taskgroup->workshare
	      && thr->task->taskgroup->prev
	      && thr->task->taskgroup->prev->cancelled)
	    return;
	}
    }
  else
    {
      ialias_call (GOMP_taskgroup_start) ();
      if (flags & GOMP_TASK_FLAG_REDUCTION)
	{
	  struct gomp_data_head { TYPE t1, t2; uintptr_t *ptr; };
	  uintptr_t *ptr = ((struct gomp_data_head *) data)->ptr;
	  ialias_call (GOMP_taskgroup_reduction_register) (ptr);
	}
    }

  if (priority > gomp_max_task_priority_var)
    priority = gomp_max_task_priority_var;

  if ((flags & GOMP_TASK_FLAG_IF) == 0 || team == NULL
      || (thr->task && thr->task->final_task)
      || team->task_count + num_tasks > 64 * team->nthreads)
    {
      unsigned long i;
      if (__builtin_expect (cpyfn != NULL, 0))
	{
	  struct gomp_task task[num_tasks];
	  struct gomp_task *parent = thr->task;
	  arg_size = (arg_size + arg_align - 1) & ~(arg_align - 1);
	  char buf[num_tasks * arg_size + arg_align - 1];
	  char *arg = (char *) (((uintptr_t) buf + arg_align - 1)
				& ~(uintptr_t) (arg_align - 1));
	  char *orig_arg = arg;
	  for (i = 0; i < num_tasks; i++)
	    {
	      gomp_init_task (&task[i], parent, gomp_icv (false));
	      task[i].priority = priority;
	      task[i].kind = GOMP_TASK_UNDEFERRED;
	      task[i].final_task = (thr->task && thr->task->final_task)
				   || (flags & GOMP_TASK_FLAG_FINAL);
	      if (thr->task)
		{
		  task[i].in_tied_task = thr->task->in_tied_task;
		  task[i].taskgroup = thr->task->taskgroup;
		}
	      thr->task = &task[i];
	      cpyfn (arg, data);
	      arg += arg_size;
	    }
	  arg = orig_arg;
	  for (i = 0; i < num_tasks; i++)
	    {
	      thr->task = &task[i];
	      ((TYPE *)arg)[0] = start;
	      start += task_step;
	      ((TYPE *)arg)[1] = start;
	      if (i == nfirst)
		task_step = nfirst_task_step;
	      fn (arg);
	      arg += arg_size;
	      if (!priority_queue_empty_p (&task[i].children_queue,
					   MEMMODEL_RELAXED))
		{
		  gomp_mutex_lock (&team->task_lock);
		  gomp_clear_parent (&task[i].children_queue);
		  gomp_mutex_unlock (&team->task_lock);
		}
	      gomp_end_task ();
	    }
	}
      else
	for (i = 0; i < num_tasks; i++)
	  {
	    struct gomp_task task;

	    gomp_init_task (&task, thr->task, gomp_icv (false));
	    task.priority = priority;
	    task.kind = GOMP_TASK_UNDEFERRED;
	    task.final_task = (thr->task && thr->task->final_task)
			      || (flags & GOMP_TASK_FLAG_FINAL);
	    if (thr->task)
	      {
		task.in_tied_task = thr->task->in_tied_task;
		task.taskgroup = thr->task->taskgroup;
	      }
	    thr->task = &task;
	    ((TYPE *)data)[0] = start;
	    start += task_step;
	    ((TYPE *)data)[1] = start;
	    if (i == nfirst)
	      task_step = nfirst_task_step;
	    fn (data);
	    if (!priority_queue_empty_p (&task.children_queue,
					 MEMMODEL_RELAXED))
	      {
		gomp_mutex_lock (&team->task_lock);
		gomp_clear_parent (&task.children_queue);
		gomp_mutex_unlock (&team->task_lock);
	      }
	    gomp_end_task ();
	  }
    }
  else
    {
      struct gomp_task *tasks[num_tasks];
      struct gomp_task *parent = thr->task;
      struct gomp_taskgroup *taskgroup = parent->taskgroup;
      char *arg;
      int do_wake;
      unsigned long i;

      for (i = 0; i < num_tasks; i++)
	{
	  struct gomp_task *task
	    = gomp_malloc (sizeof (*task) + arg_size + arg_align - 1);
	  tasks[i] = task;
	  arg = (char *) (((uintptr_t) (task + 1) + arg_align - 1)
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
	  ((TYPE *)arg)[0] = start;
	  start += task_step;
	  ((TYPE *)arg)[1] = start;
	  if (i == nfirst)
	    task_step = nfirst_task_step;
	  thr->task = parent;
	  task->kind = GOMP_TASK_WAITING;
	  task->fn = fn;
	  task->fn_data = arg;
	  task->final_task = (flags & GOMP_TASK_FLAG_FINAL) >> 1;
	}
      gomp_mutex_lock (&team->task_lock);
      /* If parallel or taskgroup has been cancelled, don't start new
	 tasks.  */
      if (__builtin_expect (gomp_cancel_var, 0)
	  && cpyfn == NULL)
	{
	  if (gomp_team_barrier_cancelled (&team->barrier))
	    {
	    do_cancel:
	      gomp_mutex_unlock (&team->task_lock);
	      for (i = 0; i < num_tasks; i++)
		{
		  gomp_finish_task (tasks[i]);
		  free (tasks[i]);
		}
	      if ((flags & GOMP_TASK_FLAG_NOGROUP) == 0)
		ialias_call (GOMP_taskgroup_end) ();
	      return;
	    }
	  if (taskgroup)
	    {
	      if (taskgroup->cancelled)
		goto do_cancel;
	      if (taskgroup->workshare
		  && taskgroup->prev
		  && taskgroup->prev->cancelled)
		goto do_cancel;
	    }
	}
      if (taskgroup)
	taskgroup->num_children += num_tasks;
      for (i = 0; i < num_tasks; i++)
	{
	  struct gomp_task *task = tasks[i];
	  priority_queue_insert (PQ_CHILDREN, &parent->children_queue,
				 task, priority,
				 PRIORITY_INSERT_BEGIN,
				 /*last_parent_depends_on=*/false,
				 task->parent_depends_on);
	  if (taskgroup)
	    priority_queue_insert (PQ_TASKGROUP, &taskgroup->taskgroup_queue,
				   task, priority, PRIORITY_INSERT_BEGIN,
				   /*last_parent_depends_on=*/false,
				   task->parent_depends_on);
	  priority_queue_insert (PQ_TEAM, &team->task_queue, task, priority,
				 PRIORITY_INSERT_END,
				 /*last_parent_depends_on=*/false,
				 task->parent_depends_on);
	  ++team->task_count;
	  ++team->task_queued_count;
	}
      gomp_team_barrier_set_task_pending (&team->barrier);
      if (team->task_running_count + !parent->in_tied_task
	  < team->nthreads)
	{
	  do_wake = team->nthreads - team->task_running_count
		    - !parent->in_tied_task;
	  if ((unsigned long) do_wake > num_tasks)
	    do_wake = num_tasks;
	}
      else
	do_wake = 0;
      gomp_mutex_unlock (&team->task_lock);
      if (do_wake)
	gomp_team_barrier_wake (&team->barrier, do_wake);
    }
  if ((flags & GOMP_TASK_FLAG_NOGROUP) == 0)
    ialias_call (GOMP_taskgroup_end) ();
}
