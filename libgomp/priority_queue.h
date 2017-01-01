/* Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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

/* Header file for a priority queue of GOMP tasks.  */

/* ?? Perhaps all the priority_tree_* functions are complex and rare
   enough to go out-of-line and be moved to priority_queue.c.  ??  */

#ifndef _PRIORITY_QUEUE_H_
#define _PRIORITY_QUEUE_H_

/* One task.  */

struct priority_node
{
  /* Next and previous chains in a circular doubly linked list for
     tasks within this task's priority.  */
  struct priority_node *next, *prev;
};

/* All tasks within the same priority.  */

struct priority_list
{
  /* Priority of the tasks in this set.  */
  int priority;

  /* Tasks.  */
  struct priority_node *tasks;

  /* This points to the last of the higher priority WAITING tasks.
     Remember that for the children queue, we have:

	parent_depends_on WAITING tasks.
	!parent_depends_on WAITING tasks.
	TIED tasks.

     This is a pointer to the last of the parent_depends_on WAITING
     tasks which are essentially, higher priority items within their
     priority.  */
  struct priority_node *last_parent_depends_on;
};

/* Another splay tree instantiation, for priority_list's.  */
typedef struct prio_splay_tree_node_s *prio_splay_tree_node;
typedef struct prio_splay_tree_s *prio_splay_tree;
typedef struct prio_splay_tree_key_s *prio_splay_tree_key;
struct prio_splay_tree_key_s {
  /* This structure must only containing a priority_list, as we cast
     prio_splay_tree_key to priority_list throughout.  */
  struct priority_list l;
};
#define splay_tree_prefix prio
#include "splay-tree.h"

/* The entry point into a priority queue of tasks.

   There are two alternate implementations with which to store tasks:
   as a balanced tree of sorts, or as a simple list of tasks.  If
   there are only priority-0 items (ROOT is NULL), we use the simple
   list, otherwise (ROOT is non-NULL) we use the tree.  */

struct priority_queue
{
  /* If t.root != NULL, this is a splay tree of priority_lists to hold
     all tasks.  This is only used if multiple priorities are in play,
     otherwise we use the priority_list `l' below to hold all
     (priority-0) tasks.  */
  struct prio_splay_tree_s t;

  /* If T above is NULL, only priority-0 items exist, so keep them
     in a simple list.  */
  struct priority_list l;
};

enum priority_insert_type {
  /* Insert at the beginning of a priority list.  */
  PRIORITY_INSERT_BEGIN,
  /* Insert at the end of a priority list.  */
  PRIORITY_INSERT_END
};

/* Used to determine in which queue a given priority node belongs in.
   See pnode field of gomp_task.  */

enum priority_queue_type
{
  PQ_TEAM,	    /* Node belongs in gomp_team's task_queue.  */
  PQ_CHILDREN,	    /* Node belongs in parent's children_queue.  */
  PQ_TASKGROUP,	    /* Node belongs in taskgroup->taskgroup_queue.  */
  PQ_IGNORED = 999
};

/* Priority queue implementation prototypes.  */

extern bool priority_queue_task_in_queue_p (enum priority_queue_type,
					    struct priority_queue *,
					    struct gomp_task *);
extern void priority_queue_dump (enum priority_queue_type,
				 struct priority_queue *);
extern void priority_queue_verify (enum priority_queue_type,
				   struct priority_queue *, bool);
extern void priority_tree_remove (enum priority_queue_type,
				  struct priority_queue *,
				  struct priority_node *);
extern struct gomp_task *priority_tree_next_task (enum priority_queue_type,
						  struct priority_queue *,
						  enum priority_queue_type,
						  struct priority_queue *,
						  bool *);

/* Return TRUE if there is more than one priority in HEAD.  This is
   used throughout to to choose between the fast path (priority 0 only
   items) and a world with multiple priorities.  */

static inline bool
priority_queue_multi_p (struct priority_queue *head)
{
  return __builtin_expect (head->t.root != NULL, 0);
}

/* Initialize a priority queue.  */

static inline void
priority_queue_init (struct priority_queue *head)
{
  head->t.root = NULL;
  /* To save a few microseconds, we don't initialize head->l.priority
     to 0 here.  It is implied that priority will be 0 if head->t.root
     == NULL.

     priority_tree_insert() will fix this when we encounter multiple
     priorities.  */
  head->l.tasks = NULL;
  head->l.last_parent_depends_on = NULL;
}

static inline void
priority_queue_free (struct priority_queue *head)
{
  /* There's nothing to do, as tasks were freed as they were removed
     in priority_queue_remove.  */
}

/* Forward declarations.  */
static inline size_t priority_queue_offset (enum priority_queue_type);
static inline struct gomp_task *priority_node_to_task
				(enum priority_queue_type,
				 struct priority_node *);
static inline struct priority_node *task_to_priority_node
				    (enum priority_queue_type,
				     struct gomp_task *);

/* Return TRUE if priority queue HEAD is empty.

   MODEL IS MEMMODEL_ACQUIRE if we should use an acquire atomic to
   read from the root of the queue, otherwise MEMMODEL_RELAXED if we
   should use a plain load.  */

static inline _Bool
priority_queue_empty_p (struct priority_queue *head, enum memmodel model)
{
  /* Note: The acquire barriers on the loads here synchronize with
     the write of a NULL in gomp_task_run_post_remove_parent.  It is
     not necessary that we synchronize with other non-NULL writes at
     this point, but we must ensure that all writes to memory by a
     child thread task work function are seen before we exit from
     GOMP_taskwait.  */
  if (priority_queue_multi_p (head))
    {
      if (model == MEMMODEL_ACQUIRE)
	return __atomic_load_n (&head->t.root, MEMMODEL_ACQUIRE) == NULL;
      return head->t.root == NULL;
    }
  if (model == MEMMODEL_ACQUIRE)
    return __atomic_load_n (&head->l.tasks, MEMMODEL_ACQUIRE) == NULL;
  return head->l.tasks == NULL;
}

/* Look for a given PRIORITY in HEAD.  Return it if found, otherwise
   return NULL.  This only applies to the tree variant in HEAD.  There
   is no point in searching for priorities in HEAD->L.  */

static inline struct priority_list *
priority_queue_lookup_priority (struct priority_queue *head, int priority)
{
  if (head->t.root == NULL)
    return NULL;
  struct prio_splay_tree_key_s k;
  k.l.priority = priority;
  return (struct priority_list *)
    prio_splay_tree_lookup (&head->t, &k);
}

/* Insert task in DATA, with PRIORITY, in the priority list in LIST.
   LIST contains items of type TYPE.

   If POS is PRIORITY_INSERT_BEGIN, the new task is inserted at the
   top of its respective priority.  If POS is PRIORITY_INSERT_END, the
   task is inserted at the end of its priority.

   If ADJUST_PARENT_DEPENDS_ON is TRUE, LIST is a children queue, and
   we must keep track of higher and lower priority WAITING tasks by
   keeping the queue's last_parent_depends_on field accurate.  This
   only applies to the children queue, and the caller must ensure LIST
   is a children queue in this case.

   If ADJUST_PARENT_DEPENDS_ON is TRUE, TASK_IS_PARENT_DEPENDS_ON is
   set to the task's parent_depends_on field.  If
   ADJUST_PARENT_DEPENDS_ON is FALSE, this field is irrelevant.

   Return the new priority_node.  */

static inline void
priority_list_insert (enum priority_queue_type type,
		      struct priority_list *list,
		      struct gomp_task *task,
		      int priority,
		      enum priority_insert_type pos,
		      bool adjust_parent_depends_on,
		      bool task_is_parent_depends_on)
{
  struct priority_node *node = task_to_priority_node (type, task);
  if (list->tasks)
    {
      /* If we are keeping track of higher/lower priority items,
	 but this is a lower priority WAITING task
	 (parent_depends_on != NULL), put it after all ready to
	 run tasks.  See the comment in
	 priority_queue_upgrade_task for a visual on how tasks
	 should be organized.  */
      if (adjust_parent_depends_on
	  && pos == PRIORITY_INSERT_BEGIN
	  && list->last_parent_depends_on
	  && !task_is_parent_depends_on)
	{
	  struct priority_node *last_parent_depends_on
	    = list->last_parent_depends_on;
	  node->next = last_parent_depends_on->next;
	  node->prev = last_parent_depends_on;
	}
      /* Otherwise, put it at the top/bottom of the queue.  */
      else
	{
	  node->next = list->tasks;
	  node->prev = list->tasks->prev;
	  if (pos == PRIORITY_INSERT_BEGIN)
	    list->tasks = node;
	}
      node->next->prev = node;
      node->prev->next = node;
    }
  else
    {
      node->next = node;
      node->prev = node;
      list->tasks = node;
    }
  if (adjust_parent_depends_on
      && list->last_parent_depends_on == NULL
      && task_is_parent_depends_on)
    list->last_parent_depends_on = node;
}

/* Tree version of priority_list_insert.  */

static inline void
priority_tree_insert (enum priority_queue_type type,
		      struct priority_queue *head,
		      struct gomp_task *task,
		      int priority,
		      enum priority_insert_type pos,
		      bool adjust_parent_depends_on,
		      bool task_is_parent_depends_on)
{
  if (__builtin_expect (head->t.root == NULL, 0))
    {
      /* The first time around, transfer any priority 0 items to the
	 tree.  */
      if (head->l.tasks != NULL)
	{
	  prio_splay_tree_node k = gomp_malloc (sizeof (*k));
	  k->left = NULL;
	  k->right = NULL;
	  k->key.l.priority = 0;
	  k->key.l.tasks = head->l.tasks;
	  k->key.l.last_parent_depends_on = head->l.last_parent_depends_on;
	  prio_splay_tree_insert (&head->t, k);
	  head->l.tasks = NULL;
	}
    }
  struct priority_list *list
    = priority_queue_lookup_priority (head, priority);
  if (!list)
    {
      prio_splay_tree_node k = gomp_malloc (sizeof (*k));
      k->left = NULL;
      k->right = NULL;
      k->key.l.priority = priority;
      k->key.l.tasks = NULL;
      k->key.l.last_parent_depends_on = NULL;
      prio_splay_tree_insert (&head->t, k);
      list = &k->key.l;
    }
  priority_list_insert (type, list, task, priority, pos,
			adjust_parent_depends_on,
			task_is_parent_depends_on);
}

/* Generic version of priority_*_insert.  */

static inline void
priority_queue_insert (enum priority_queue_type type,
		       struct priority_queue *head,
		       struct gomp_task *task,
		       int priority,
		       enum priority_insert_type pos,
		       bool adjust_parent_depends_on,
		       bool task_is_parent_depends_on)
{
#if _LIBGOMP_CHECKING_
  if (priority_queue_task_in_queue_p (type, head, task))
    gomp_fatal ("Attempt to insert existing task %p", task);
#endif
  if (priority_queue_multi_p (head) || __builtin_expect (priority > 0, 0))
    priority_tree_insert (type, head, task, priority, pos,
			  adjust_parent_depends_on,
			  task_is_parent_depends_on);
  else
    priority_list_insert (type, &head->l, task, priority, pos,
			  adjust_parent_depends_on,
			  task_is_parent_depends_on);
}

/* If multiple priorities are in play, return the highest priority
   task from within Q1 and Q2, while giving preference to tasks from
   Q1.  If the returned task is chosen from Q1, *Q1_CHOSEN_P is set to
   TRUE, otherwise it is set to FALSE.

   If multiple priorities are not in play (only 0 priorities are
   available), the next task is chosen exclusively from Q1.

   As a special case, Q2 can be NULL, in which case, we just choose
   the highest priority WAITING task in Q1.  This is an optimization
   to speed up looking through only one queue.

   We assume Q1 has at least one item.  */

static inline struct gomp_task *
priority_queue_next_task (enum priority_queue_type t1,
			  struct priority_queue *q1,
			  enum priority_queue_type t2,
			  struct priority_queue *q2,
			  bool *q1_chosen_p)
{
#if _LIBGOMP_CHECKING_
  if (priority_queue_empty_p (q1, MEMMODEL_RELAXED))
    gomp_fatal ("priority_queue_next_task: Q1 is empty");
#endif
  if (priority_queue_multi_p (q1))
    {
      struct gomp_task *t
	= priority_tree_next_task (t1, q1, t2, q2, q1_chosen_p);
      /* If T is NULL, there are no WAITING tasks in Q1.  In which
	 case, return any old (non-waiting) task which will cause the
	 caller to do the right thing when checking T->KIND ==
	 GOMP_TASK_WAITING.  */
      if (!t)
	{
#if _LIBGOMP_CHECKING_
	  if (*q1_chosen_p == false)
	    gomp_fatal ("priority_queue_next_task inconsistency");
#endif
	  return priority_node_to_task (t1, q1->t.root->key.l.tasks);
	}
      return t;
    }
  else
    {
      *q1_chosen_p = true;
      return priority_node_to_task (t1, q1->l.tasks);
    }
}

/* Remove NODE from LIST.

   If we are removing the one and only item in the list, and MODEL is
   MEMMODEL_RELEASE, use an atomic release to clear the list.

   If the list becomes empty after the remove, return TRUE.  */

static inline bool
priority_list_remove (struct priority_list *list,
		      struct priority_node *node,
		      enum memmodel model)
{
  bool empty = false;
  node->prev->next = node->next;
  node->next->prev = node->prev;
  if (list->tasks == node)
    {
      if (node->next != node)
	list->tasks = node->next;
      else
	{
	  /* We access task->children in GOMP_taskwait outside of
	     the task lock mutex region, so need a release barrier
	     here to ensure memory written by child_task->fn above
	     is flushed before the NULL is written.  */
	  if (model == MEMMODEL_RELEASE)
	    __atomic_store_n (&list->tasks, NULL, MEMMODEL_RELEASE);
	  else
	    list->tasks = NULL;
	  empty = true;
	  goto remove_out;
	}
    }
remove_out:
#if _LIBGOMP_CHECKING_
  memset (node, 0xaf, sizeof (*node));
#endif
  return empty;
}

/* This is the generic version of priority_list_remove.

   Remove NODE from priority queue HEAD.  HEAD contains tasks of type TYPE.

   If we are removing the one and only item in the priority queue and
   MODEL is MEMMODEL_RELEASE, use an atomic release to clear the queue.

   If the queue becomes empty after the remove, return TRUE.  */

static inline bool
priority_queue_remove (enum priority_queue_type type,
		       struct priority_queue *head,
		       struct gomp_task *task,
		       enum memmodel model)
{
#if _LIBGOMP_CHECKING_
  if (!priority_queue_task_in_queue_p (type, head, task))
    gomp_fatal ("Attempt to remove missing task %p", task);
#endif
  if (priority_queue_multi_p (head))
    {
      priority_tree_remove (type, head, task_to_priority_node (type, task));
      if (head->t.root == NULL)
	{
	  if (model == MEMMODEL_RELEASE)
	    /* Errr, we store NULL twice, the alternative would be to
	       use an atomic release directly in the splay tree
	       routines.  Worth it?  */
	    __atomic_store_n (&head->t.root, NULL, MEMMODEL_RELEASE);
	  return true;
	}
      return false;
    }
  else
    return priority_list_remove (&head->l,
				 task_to_priority_node (type, task), model);
}

#endif /* _PRIORITY_QUEUE_H_ */
