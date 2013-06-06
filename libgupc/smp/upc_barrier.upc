/* Copyright (C) 2001-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * UPC barrier implementation support routines.
 *
 * The UPC barrier synchronization statements are:
 *  - upc_notify <i>expression</i>
 *  - upc_wait <i>expression</i>
 *  - upc_barrier <i>expression</i>
 *
 * The upc barrier statement is equivalent to the compound statement:
 *   <i>{ upc_notify barrier_value; upc_wait barrier_value; }</i>
 *
 * The UPC runtime implementation of the barrier organizes the UPC
 * threads in a form of a tree with a configurable tree fanout.  Each
 * thread uses the following data structures:
 *
 * * A shared array of barrier block structures. Each thread has the
 *   barrier block structure consisting of the following variables:
 *   - notify    - Atomically incremented by the thread and its children
 *               whenever they arrive on the notify statement.  Once all
 *		 of them arrive, the parent of the thread is notified.
 *   - wait      - Signaling field for parent to inform children that they
 *		 are allowed to proceed from the wait phase.
 *   - id[2]     - Barrier ID that thread is waiting on. There are two
 *               barrier IDs to distinguish the correct notify/barrier
 *               sequence (a thread can be in a notify phase while children
 *               are still in the previous barrier wait state and need the
 *		 parent's barrier ID to compare against their own).
 * * A local array of threads' notify counts required to complete the notify
 *   phase (as we use atomic fetch and add function the required number of
 *   of notifications is equal to the children count).
 *
 * BARRIER NOTIFY
 *
 *  * Each leaf thread atomically increments the 'notify' field of the parent's
 *    barrier block.  Others atomically increment the same filed in their own
 *    barrier block.  The number of notifies before increment is returned back.
 *  * If number of notifies is equal to the thread's notify count the parent
 *    of the thread must be notified.  This propagates notification to the
 *    top of the tree (no thread waits for anyone in the notify phase).  Before
 *    parent is notified, the MAX barrier ID of the thread and its children
 *    is calculated and set as the effective thread's barrier ID.  At the
 *    end, the root thread has the MAX calculated ID for all threads.
 *  * The last thread notifying the root thread is also responsible for
 *    releasing the root thread from the wait.
 *
 * BARRIER WAIT
 *
 *  * Each parent (or another thread on behalf of the parent) atomically
 *    increments the 'wait' filed of its children (all threads completed
 *    the notify phase)..
 *  * Each thread also atomically increments the 'wait' field in its own
 *    barrier block (thread arrived on the wait statement).
 *  * Whoever incremented the filed first is responsible for further
 *    releasing thread's children. This makes sure that all children of a
 *    thread that has not arrived on the wait statement are allowed
 *    to complete their wait statements (split phase barrier).
 *
 * Current limitations:
 *   - Recursive behavior in the notify and wait statements can lead into
 *     more work for some of the threads.
 */

#include <upc.h>
#include <stdlib.h>
#include <stdio.h>
#include "config.h"

/* Thread's children.  */
static int *__upc_child;
/* Thread's children count.  */
static int __upc_child_cnt;
/* Thread's parent thread.  */
static int __upc_parent;

/* Thread tree definitions.  */
#define ROOT_PARENT	-1
#define ROOT_NODE	0
#define INNER_NODE	1
#define LEAF_NODE	2
#define ROOT_THREAD	(__upc_node == ROOT_NODE)
#define INNER_THREAD	(__upc_node == INNER_NODE)
#define LEAF_THREAD	(__upc_node == LEAF_NODE)
int __upc_node;

/* Notify counts for each thread.  */
int *__upc_notify_cnt;

/* Per thread barrier structure.  */
struct barrier_block
{
  int notify;
  int wait;
  int id[2];
};

typedef struct barrier_block barrier_block_t;
shared barrier_block_t __upc_btree[THREADS];
/* Alternative barrier count (even/odd). Need to distinguish barrier IDs from
   two consecutive barriers as some threads might enter the notify statements
   while the others have not completed the wait statement of the previous
   barrier.  */
static int __upc_bphase = 0;
/* Atomic increment values.  */
/* Thread arrived first on the wait (parent trying to release
   children, or thread waiting for the parent.  */
#define GUPCR_BARRIER_FIRST_ON_WAIT 0
/* Both parent and thread arrived.  */
#define GUPCR_BARRIER_WAIT_COMPLETED 2

/* Per-thread flag set by upc_notify() and cleared by upc_wait().  */
static GUPCR_THREAD_LOCAL int __upc_barrier_active = 0;

/* Per-thread active barrier ID.  */
GUPCR_THREAD_LOCAL int __upc_barrier_id = 0;

/*
 * Shared integer atomic increment.
 *
 */
__attribute__ ((__always_inline__))
static inline
int
__upc_atomic_inc (shared void *p)
{
  int *addr = __upc_map_to_local (p);
  return __upc_sync_fetch_and_add (addr, 1);
}

/*
 * Adjust thread's barrier ID.
 *
 * The MAX barrier ID among all threads is being propagated
 * to the top of the tree. Adjust barrier ID of the thread
 * to the MAX among the thread and its children.
 *
 */
__attribute__ ((__always_inline__))
static inline
void
__upc_adjust_barrier_id (int thread)
{
  int i, maxbid;
  maxbid = __upc_btree[thread].id[__upc_bphase];
  for (i = 0; i < GUPCR_TREE_FANOUT; i++)
    {
      int child = GUPCR_TREE_FANOUT * thread + i + 1;
      if (child < THREADS)
	{
	  int bid = __upc_btree[child].id[__upc_bphase];
	  if (maxbid < bid)
	    maxbid = bid;
	}
    }
  __upc_btree[thread].id[__upc_bphase] = maxbid;
}

/*
 * Release waiting thread.
 *
 * Signal to the specified thread that it can complete
 * the wait phase.
 *
 * This is a recursive function. If the specified thread did not
 * arrive on the wait 'gate', the calling thread must release
 * all its children with atomic inc into their wait fileds.
 *
 */
static inline
void
__upc_release_wait (int thread)
{
  int wait_cnt = __upc_atomic_inc (&__upc_btree[thread].wait);
  if (wait_cnt == GUPCR_BARRIER_FIRST_ON_WAIT)
    {
      int i;
      /* Parent arrived first.  Make agreed on MAX barrier ID available
	 to children before releasing them.  */
      if (INNER_THREAD)
	__upc_btree[thread].id[__upc_bphase] =
	  __upc_btree[(thread - 1) / GUPCR_TREE_FANOUT].id[__upc_bphase];
      for (i = 0; i < GUPCR_TREE_FANOUT; i++)
	{
	  int child = GUPCR_TREE_FANOUT * thread + i + 1;
	  if (child < THREADS)
	    {
	      __upc_release_wait (child);
	    }
	}
    }
}

/*
 * UPC notify statement implementation.
 */
void
__upc_notify (int barrier_id)
{
  int notify_cnt;
  int notify_thread;
  if (__upc_barrier_active)
    __upc_fatal ("Two successive upc_notify statements executed "
		 "without an intervening upc_wait");
  __upc_barrier_active = 1;
  __upc_barrier_id = barrier_id;

  /* Initialize thread's barrier block.  */
  __upc_btree[MYTHREAD].id[__upc_bphase] = barrier_id;
  __upc_btree[MYTHREAD].wait = 0;

  /* Notify that thread arrived.  */
  if (LEAF_THREAD)
    notify_thread = __upc_parent;
  else
    notify_thread = MYTHREAD;
  notify_cnt = __upc_atomic_inc (&__upc_btree[notify_thread].notify);
  if (notify_cnt == __upc_notify_cnt[notify_thread])
    {
      /* Notify count reached the expected notification count (thread
	 and all its children arrived on notification phase).
	 Must traverse the tree and inform parent of the thread.  */
      do
	{
	  __upc_btree[notify_thread].notify = 0;
	  /* Adjust the barrier ID with the MAX of the
	     thread and its children.  */
	  __upc_adjust_barrier_id (notify_thread);
	  if (notify_thread == 0)
	    {
	      /* Reached the top of the tree.  Release the root
		 thread from the wait.  */
	      __upc_release_wait (notify_thread);
	      break;
	    }
	  /* The parent of the thread is the new thread that has
	     to be notified.  */
	  notify_thread = (notify_thread - 1) / GUPCR_TREE_FANOUT;
	}
      while (__upc_notify_cnt[notify_thread] ==
	     __upc_atomic_inc (&__upc_btree[notify_thread].notify));
    }
}

/*
 * UPC wait statement implementation
 */
void
__upc_wait (int barrier_id)
{
  int wait_cnt, i;

  if (!__upc_barrier_active)
    __upc_fatal ("upc_wait statement executed without a "
		 "preceding upc_notify");
  /* Check the barrier ID with the one from the notify phase.  */
  if (barrier_id != INT_MIN && __upc_barrier_id != INT_MIN &&
      __upc_barrier_id != barrier_id)
    {
      __upc_fatal ("UPC barrier identifier mismatch");
    }

  /* Announce the thread on the wait phase.  */
  wait_cnt = __upc_atomic_inc (&__upc_btree[MYTHREAD].wait);
  if (wait_cnt == GUPCR_BARRIER_FIRST_ON_WAIT)
    {
      /* Must wait for the parent.  */
      int *wait_ptr = (int *) &__upc_btree[MYTHREAD].wait;
      __upc_spin_until (*wait_ptr == GUPCR_BARRIER_WAIT_COMPLETED);
    }

  if (wait_cnt == GUPCR_BARRIER_FIRST_ON_WAIT)
    {
      /* Thread arrived before parent and waited for the release
	 from the parent.  Release all the children from the wait
	 and make agreed on MAX barrier ID available to them.  */
      if (INNER_THREAD)
	__upc_btree[MYTHREAD].id[__upc_bphase] =
	  __upc_btree[__upc_parent].id[__upc_bphase];
      for (i = 0; i < __upc_child_cnt; i++)
	__upc_release_wait (__upc_child[i]);
    }

  /* Compare barrier ID with parent's barrier ID.  */
  if (barrier_id != INT_MIN)
    {
      int exp;
      if (ROOT_THREAD)
	exp = __upc_btree[MYTHREAD].id[__upc_bphase];
      else
        exp = __upc_btree[__upc_parent].id[__upc_bphase];
      if (exp != INT_MIN && exp != barrier_id)
        {
	  __upc_fatal ("UPC barrier identifier mismatch");
	}
    }

  __upc_barrier_active = 0;
  if (__upc_bphase)
    __upc_bphase = 0;
  else
    __upc_bphase = 1;
  upc_fence;
}

/* 
 * UPC barrier implementation.
 */
void
__upc_barrier (int barrier_id)
{
  __upc_notify (barrier_id);
  __upc_wait (barrier_id);
}

/*
 * Initialize barrier.
 *
 * Initialize barrier data structures. A node tree is
 * used to signal/ack thread's arrival on the barrier.
 */
void
__upc_barrier_init (void)
{
  int i, thread;

  /* Allocate space for children thread numbers.  */
  __upc_child = malloc (GUPCR_TREE_FANOUT * sizeof (int));

  /* Find all children of this thread. */
  for (i = 0; i < GUPCR_TREE_FANOUT; i++)
    {
      int child = GUPCR_TREE_FANOUT * MYTHREAD + i + 1;
      if (child < THREADS)
	{
	  __upc_child_cnt++;
	  __upc_child[i] = child;
	}
    }
  if (MYTHREAD == 0)
    __upc_parent = ROOT_PARENT;
  else
    __upc_parent = (MYTHREAD - 1) / GUPCR_TREE_FANOUT;

  /* Set the node assignment for this thread.  */
  if (!MYTHREAD)
    __upc_node = ROOT_NODE;
  else if (__upc_child_cnt)
    __upc_node = INNER_NODE;
  else
    __upc_node = LEAF_NODE;

  /* Calculate notifications for each thread. Equal to children
     count as atomic fetch and add is used.  */
  __upc_notify_cnt = malloc (THREADS * sizeof (int));
  if (!__upc_notify_cnt)
    __upc_fatal
      ("UPC barrier initialization failed - cannot allocate memory");
  for (thread = 0; thread < THREADS; thread++)
    {
      __upc_notify_cnt[thread] = 0;
      for (i = 0; i < GUPCR_TREE_FANOUT; i++)
	{
	  int child = GUPCR_TREE_FANOUT * thread + i + 1;
	  if (child < THREADS)
	    {
	      __upc_notify_cnt[thread]++;
	    }
	}
    }
}

