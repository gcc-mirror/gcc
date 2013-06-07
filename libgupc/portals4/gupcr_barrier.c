/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

/**
 * @file gupcr_barrier.c
 * GUPC Portals4 barrier implementation.
 *
 * The UPC barrier synchronization statements are:
 *  - upc_notify <i>expression</i>
 *  - upc_wait <i>expression</i>
 *  - upc_barrier <i>expression</i>
 *
 * The upc barrier statement is equivalent to the compound statement:
 *   <i>{ upc_notify barrier_value; upc_wait barrier_value; }</i>
 *
 * Important rules:
 *  - Each thread executes an alternating sequence of upc_notify and upc_wait
 *    statements.
 *  - A thread completes a <i>upc_wait</i> statement when all threads
 *    have executed a matching <i>upc_notify</i> statement.
 *  - <i>upc_notify</i> and <i>upc_wait</i> are collective operations and
 *    <i>expression</i> (if available) must match across all threads.
 *  - An empty <i>expression</i> matches any barrier ID.
 *
 * The GUPC runtime barrier implementation uses an "all reduce"
 * algorithm as outlined in the paper <i>Enabling Flexible Collective
 * Communication Offload with Triggered Operations</i> by Keith Underwood
 * et al. January, 2007.  Portals atomic operations and triggered
 * atomic operations are used to propagate and verify
 * that all UPC threads have entered the same synchronization phase
 * with matching barrier IDs.
 *
 * For the purposes of implementing GUPC barriers, all UPC threads
 * in a given job are organized as a tree.  Thread 0 is the
 * root thread (at the top of the tree). Other threads can be
 * either an inner thread (has at least one child), or a leaf
 * thread (has no children).
 *
 * A UPC barrier is implemented in two distinctive steps: notify and wait.
 *
 * A notify step uses the GUPCR_PTL_PTE_BARRIER_UP PTE to pass
 * its barrier ID to the parent.  The result of an atomic PTL_MIN
 * operation among children and their parent is passed to the
 * parent's parent until thread 0 is reached.
 *
 * A wait step uses the GUPCR_PTL_PTE_BARRIER_DOWN PTE to pass
 * the derived consensus barrier ID to all threads.  An error
 * is raised if the derived ID does not match the thread's barrier ID.
 *
 * This implementation supports a split phase barrier where a given
 * thread completes its wait statement once all other threads
 * have reached their matching notify statement.
 *
 * Each thread uses the following resources:
 *
 *   - PTEs (and LEs) for passing barrier IDs UP and DOWN the tree
 *   - MDs for sending a thread's barrier ID to parents and children
 *   - Counting events for LEs and MDs
 *   - Event queues for failure events on LEs and MDs
 *
 * Extensive use of Portals triggered functions allow for the efficient
 * implementation of a split phase barrier.
 *
 * @addtogroup BARRIER GUPCR Barrier Functions
 * @{
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_sync.h"
#include "gupcr_broadcast.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"

/** Per-thread flag set by upc_notify() and cleared by upc_wait() */
static int gupcr_barrier_active = 0;

/** Max barrier ID used by the barrier implementation.
 * The Portals PTL_MIN atomic function is used by
 * each thread to report its barrier ID to its parents.
 * The MAX barrier ID value is used to initialize the memory
 * location targeted by PTL_MIN function.
 */
#define BARRIER_ID_MAX INT_MAX
/** Anonymous barrier ID used by the barrier implementation.
 * This barrier ID is used for barrier statements that do not
 * specify a barrier ID and it matches any other barrier ID.
 */
#define BARRIER_ANONYMOUS INT_MIN
/** Size of the barrier ID */
#define BARRIER_ID_SIZE (sizeof (gupcr_barrier_value))

/** Leaf thread check */
#define LEAF_THREAD  ((THREADS != 1) && (gupcr_child_cnt == 0))
/** Root thread check */
#define ROOT_THREAD  (gupcr_parent_thread == -1)
/** Inner thread check */
#define INNER_THREAD ((gupcr_child_cnt != 0) && (gupcr_parent_thread != -1))

/** Thread's current barrier ID */
static int gupcr_barrier_id;

/** Memory storage for notify barrier ID.  Mapped by
    LE for external access, and MD for internal access.  */

static int gupcr_notify_value;
/** Barrier notify LE handle (appended to GUPCR_PTL_PTE_BARRIER_UP) */
static ptl_handle_le_t gupcr_notify_le;
/** Barrier notify LE EQ handle */
static ptl_handle_eq_t gupcr_notify_le_eq;
/** Barrier notify LE CT handle */
static ptl_handle_ct_t gupcr_notify_le_ct;
/** Barrier notify LE CT wait counter */
static ptl_size_t gupcr_notify_le_count;
/** Barrier notify MD handle */
static ptl_handle_md_t gupcr_notify_md;
/** Barrier notify MD EQ handle */
static ptl_handle_eq_t gupcr_notify_md_eq;
/** Barrier notify MD CT handle */
static ptl_handle_ct_t gupcr_notify_md_ct;
/** Barrier notify MD CT wait counter */
static ptl_size_t gupcr_notify_md_count;

/** Barrier wait memory buffer pointer.  The buffer is
    mapped by a Portals LE for external access, and a Portals
    MD for internal access.  A pointer to the buffer is needed
    because the broadcast implementation used internally by the
    UPC runtime can broadcast arbitrarily sized values (that may
    be lareger than an 'int'.  */
static int *gupcr_wait_ptr;
/** Barrier wait LE handle (appended to GUPCR_PTL_PTE_BARRIER_DOWN) */
static ptl_handle_le_t gupcr_wait_le;
/** Barrier wait LE CT handle */
static ptl_handle_ct_t gupcr_wait_le_ct;
/** Barrier wait LE CT wait counter */
static ptl_size_t gupcr_wait_le_count;
/** Barrier wait LE EQ handle */
static ptl_handle_eq_t gupcr_wait_le_eq;
/** Barrier wait MD handle */
static ptl_handle_md_t gupcr_wait_md;
/** Barrier wait MD CT handle */
static ptl_handle_ct_t gupcr_wait_md_ct;
/** Barrier wait MD CT wait counter */
static ptl_size_t gupcr_wait_md_count;
/** Barrier wait MD EQ handle */
static ptl_handle_eq_t gupcr_wait_md_eq;

/** Memory storage (notify source) that holds the barrier ID for the PTL_MIN
    atomic function used in the notify phase of the barrier.  */
static int gupcr_barrier_value;
/** Barrier MD handle for the notify source */
static ptl_handle_md_t gupcr_barrier_md;
/** Barrier CT handle for the notify source */
static ptl_handle_ct_t gupcr_barrier_md_ct;
/** Barrier CT handle for the notify source wait counter */
static ptl_size_t gupcr_barrier_md_count;
/** Barrier EQ handle for the notify source */
static ptl_handle_eq_t gupcr_barrier_md_eq;

/** Memory storage that holds the maximum barrier ID value used to
    re-initialize the memory storage for the notify barrier ID.  */
static int gupcr_barrier_max_value = BARRIER_ID_MAX;
/** Barrier MD for MAX re-init */
static ptl_handle_md_t gupcr_barrier_max_md;
/** Barrier CT handle for MAX re-init */
static ptl_handle_ct_t gupcr_barrier_max_md_ct;
/** Barrier CT handle for MAX re-init wait counter */
static ptl_size_t gupcr_barrier_max_md_count;
/** Barrier EQ handle for MAX re-init */
static ptl_handle_eq_t gupcr_barrier_max_md_eq;

/**
 * @fn __upc_notify (int barrier_id)
 * UPC <i>upc_notify<i> statement implementation
 *
 * This procedure sets the necessary Portals triggers to implement
 * the pass that derives a consensus barrier ID value across all
 * UPC threads.  The inner threads use Portals triggered operations
 * to pass the barrier ID negotiated among itself and its children
 * up the tree its parent.
 * @param [in] barrier_id Barrier ID
 */
void
__upc_notify (int barrier_id)
{
  ptl_process_t rpid __attribute ((unused));

  gupcr_trace (FC_BARRIER, "BARRIER NOTIFY ENTER %d", barrier_id);

  if (gupcr_barrier_active)
    gupcr_error ("two successive upc_notify statements executed "
		 "without an intervening upc_wait");
  gupcr_barrier_active = 1;
  gupcr_barrier_id = barrier_id;

  /* The UPC shared memory consistency model requires all outstanding
     read/write operations to complete on the thread's
     current synchronization phase.  */
  gupcr_gmem_sync ();

#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
  if (THREADS == 1)
    return;

  /* Use barrier MAX number if barrier ID is "match all"
     This effectively excludes the thread from setting the min ID
     among the threads.  */
  gupcr_barrier_value = (barrier_id == BARRIER_ANONYMOUS) ?
    BARRIER_ID_MAX : barrier_id;

  if (gupcr_debug_enabled (FC_BARRIER))
    {
      ptl_ct_event_t ct;
      gupcr_portals_call (PtlCTGet, (gupcr_wait_le_ct, &ct));
      gupcr_debug (FC_BARRIER, "Wait LE counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_wait_le_count);
      gupcr_portals_call (PtlCTGet, (gupcr_wait_md_ct, &ct));
      gupcr_debug (FC_BARRIER, "Wait MD counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_wait_md_count);
      gupcr_portals_call (PtlCTGet, (gupcr_notify_le_ct, &ct));
      gupcr_debug (FC_BARRIER, "Notify LE counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_notify_le_count);
      gupcr_portals_call (PtlCTGet, (gupcr_notify_md_ct, &ct));
      gupcr_debug (FC_BARRIER, "Notify MD counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_notify_md_count);
      gupcr_portals_call (PtlCTGet, (gupcr_barrier_md_ct, &ct));
      gupcr_debug (FC_BARRIER, "Barrier MD counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_barrier_md_count);
      gupcr_portals_call (PtlCTGet, (gupcr_barrier_max_md_ct, &ct));
      gupcr_debug (FC_BARRIER, "Barrier max MD counter: %lu (%lu)",
		   (long unsigned) ct.success,
		   (long unsigned) gupcr_barrier_max_md_count);
    }

  if (LEAF_THREAD)
    {
      /* Send the barrier ID to the parent - use atomic PTL_MIN to allow
         parent to find the minimum barrier ID among itself and its
         children.  */
      gupcr_debug (FC_BARRIER, "Send atomic PTL_MIN %d to (%d)",
		   gupcr_barrier_value, gupcr_parent_thread);
      rpid.rank = gupcr_parent_thread;
      gupcr_portals_call (PtlAtomic, (gupcr_barrier_md, 0,
				      BARRIER_ID_SIZE, PTL_NO_ACK_REQ,
				      rpid, GUPCR_PTL_PTE_BARRIER_UP,
				      PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
				      PTL_NULL_HDR_DATA, PTL_MIN,
				      PTL_INT32_T));
    }
  else
    {
      int i;
      if (ROOT_THREAD)
	{
	  /* The consensus MIN barrier ID derived in the notify (UP) phase
	     must be transferred to the wait LE for delivery to all children.
	     Trigger: Barrier ID received in the notify phase.
	     Action: Send the barrier ID to the wait buffer of the
	     barrier DOWN LE.  */
	  rpid.rank = MYTHREAD;
	  gupcr_notify_le_count += gupcr_child_cnt + 1;
	  gupcr_portals_call (PtlTriggeredPut, (gupcr_notify_md, 0,
						BARRIER_ID_SIZE,
						PTL_NO_ACK_REQ, rpid,
						GUPCR_PTL_PTE_BARRIER_DOWN,
						PTL_NO_MATCH_BITS, 0,
						PTL_NULL_USER_PTR,
						PTL_NULL_HDR_DATA,
						gupcr_notify_le_ct,
						gupcr_notify_le_count));

	}
      else
	{
	  /* The consensus MIN barrier ID of the inner thread and its children
	     is sent to the parent UPC thread.
	     Trigger: All children and this thread execute an atomic PTL_MIN
	     using each thread's UP LE.
	     Action: Transfer the consensus minimum barrier ID to the
	     this thread's parent.  */
	  rpid.rank = gupcr_parent_thread;
	  gupcr_notify_le_count += gupcr_child_cnt + 1;
	  gupcr_portals_call (PtlTriggeredAtomic, (gupcr_notify_md, 0,
						   BARRIER_ID_SIZE,
						   PTL_NO_ACK_REQ, rpid,
						   GUPCR_PTL_PTE_BARRIER_UP,
						   PTL_NO_MATCH_BITS, 0,
						   PTL_NULL_USER_PTR,
						   PTL_NULL_HDR_DATA,
						   PTL_MIN, PTL_INT32_T,
						   gupcr_notify_le_ct,
						   gupcr_notify_le_count));
	}

      /* Trigger: Barrier ID received in the wait buffer.
         Action: Reinitialize the barrier UP ID to barrier MAX value
         for the next call to upc_notify.  */
      rpid.rank = MYTHREAD;
      gupcr_wait_le_count += 1;
      gupcr_portals_call (PtlTriggeredPut, (gupcr_barrier_max_md, 0,
					    BARRIER_ID_SIZE,
					    PTL_NO_ACK_REQ, rpid,
					    GUPCR_PTL_PTE_BARRIER_UP,
					    PTL_NO_MATCH_BITS, 0,
					    PTL_NULL_USER_PTR,
					    PTL_NULL_HDR_DATA,
					    gupcr_wait_le_ct,
					    gupcr_wait_le_count));

      /* Trigger: The barrier ID is reinitialized to MAX.
         Action: Send the consensus barrier ID to all children.  */
      gupcr_notify_le_count += 1;
      for (i = 0; i < gupcr_child_cnt; i++)
	{
	  rpid.rank = gupcr_child[i];
	  gupcr_portals_call (PtlTriggeredPut, (gupcr_wait_md, 0,
						BARRIER_ID_SIZE,
						PTL_OC_ACK_REQ, rpid,
						GUPCR_PTL_PTE_BARRIER_DOWN,
						PTL_NO_MATCH_BITS, 0,
						PTL_NULL_USER_PTR,
						PTL_NULL_HDR_DATA,
						gupcr_notify_le_ct,
						gupcr_notify_le_count));
	}

      /* Allow notify to proceed and to possibly complete the wait
         phase on other threads.  */

      /* Find the minimum barrier ID among children and the root.  */
      gupcr_debug (FC_BARRIER, "Send atomic PTL_MIN %d to (%d)",
		   gupcr_barrier_value, MYTHREAD);
      rpid.rank = MYTHREAD;
      gupcr_portals_call (PtlAtomic, (gupcr_barrier_md, 0,
				      BARRIER_ID_SIZE, PTL_NO_ACK_REQ,
				      rpid, GUPCR_PTL_PTE_BARRIER_UP,
				      PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
				      PTL_NULL_HDR_DATA, PTL_MIN,
				      PTL_INT32_T));
    }
#else
  /* The UPC runtime barrier implementation that does not use
     Portals triggered operations does not support split phase barriers.
     In this case, all Portals actions related to the barrier
     are performed in the __upc_wait() function.  */
#endif
  gupcr_trace (FC_BARRIER, "BARRIER NOTIFY EXIT %d", barrier_id);
}

/**
 * @fn __upc_wait (int barrier_id)
 * UPC <i>upc_wait</i> statement implementation
 *
 * This procedure waits to receive the derived consensus
 * barrier ID from the parent (leaf thread) or acknowledges that
 * all children received the consensus barrier ID (inner
 * and root threads).  The consensus barrier ID is checked
 * against the barrier ID passed in as an argument.
 * @param [in] barrier_id Barrier ID
 */
void
__upc_wait (int barrier_id)
{
  ptl_ct_event_t ct;
  ptl_process_t rpid __attribute ((unused));
  int received_barrier_id;
  gupcr_trace (FC_BARRIER, "BARRIER WAIT ENTER %d", barrier_id);

  if (!gupcr_barrier_active)
    gupcr_error ("upc_wait statement executed without a "
		 "preceding upc_notify");

  /* Check if notify/wait barrier IDs match.
     BARRIER_ANONYMOUS matches any other barrier ID.  */
  if ((barrier_id != BARRIER_ANONYMOUS &&
       gupcr_barrier_id != BARRIER_ANONYMOUS) &&
      (gupcr_barrier_id != barrier_id))
    {
      gupcr_error ("UPC barrier identifier mismatch - notify %d, wait %d",
		   gupcr_barrier_id, barrier_id);
    }

  if (THREADS == 1)
    {
      gupcr_barrier_active = 0;
      return;
    }

#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
  /* Wait for the barrier ID to propagate down the tree.  */
  if (gupcr_child_cnt)
    {
      /* Wait for the barrier ID to flow down to the children.  */
      gupcr_wait_md_count += gupcr_child_cnt;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_md_ct, gupcr_wait_md_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_md_eq);
	  gupcr_fatal_error ("received an error on wait MD");
	}
    }
  else
    {
      gupcr_wait_le_count += 1;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_le_ct, gupcr_wait_le_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_le_eq);
	  gupcr_fatal_error ("received an error on wait LE");
	}
    }
  received_barrier_id = *gupcr_wait_ptr;
#else
  /* UPC Barrier implementation without Portals Triggered Functions.  */

  /* NOTIFY - Propagate minimal barrier ID to the root thread.  */

  /* Use the barrier maximum ID number if the barrier ID is "match all".
     This effectively excludes the thread from setting the minimum ID
     among the threads.  */
  gupcr_barrier_value = (barrier_id == BARRIER_ANONYMOUS) ?
    BARRIER_ID_MAX : barrier_id;

  if (!LEAF_THREAD)
    {
      /* This step is performed by the root thread and inner threads.  */
      /* Find the minimal barrier ID among the thread and children.
         Use the Portals PTL_MIN atomic operation on the value
	 in the notify LE.  */
      gupcr_debug (FC_BARRIER, "Send atomic PTL_MIN %d to (%d)",
		   gupcr_barrier_value, MYTHREAD);
      rpid.rank = MYTHREAD;
      gupcr_portals_call (PtlAtomic, (gupcr_barrier_md, 0,
				      BARRIER_ID_SIZE, PTL_NO_ACK_REQ,
				      rpid, GUPCR_PTL_PTE_BARRIER_UP,
				      PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
				      PTL_NULL_HDR_DATA, PTL_MIN,
				      PTL_INT32_T));
      /* Wait for all children threads to report their barrier IDs.
         Account for this thread's atomic PTL_MIN.  */
      gupcr_notify_le_count += gupcr_child_cnt + 1;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_notify_le_ct, gupcr_notify_le_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_notify_le_eq);
	  gupcr_fatal_error ("received an error on notify LE");
	}
    }

  if (!ROOT_THREAD)
    {
      ptl_handle_md_t source_md;

      /* This step is performed by leaf threads and inner threads.  */
      /* Send the barrier ID to the parent - use atomic PTL_MIN on the value
         in the parents notify LE (derived minimal ID for the parent and its
         children.  */
      gupcr_debug (FC_BARRIER, "Send atomic PTL_MIN %d to (%d)",
		   gupcr_barrier_value, gupcr_parent_thread);
      if (LEAF_THREAD)
	source_md = gupcr_barrier_md;
      else
	/* An inner thread uses the minimal barrier ID
	   derived from the parent thread and all its children.  */
	source_md = gupcr_notify_md;
      rpid.rank = gupcr_parent_thread;
      gupcr_portals_call (PtlAtomic,
			  (source_md, 0, BARRIER_ID_SIZE, PTL_NO_ACK_REQ,
			   rpid, GUPCR_PTL_PTE_BARRIER_UP,
			   PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
			   PTL_NULL_HDR_DATA, PTL_MIN, PTL_INT32_T));
    }

  /* At this point, the derived minimal barrier ID among all threads
     has arrived at the root thread.  */
  if (ROOT_THREAD)
    {
      *(int *) gupcr_wait_ptr = gupcr_notify_value;
    }
  else
    {
      /* Wait for the parent to send the derived agreed on barrier ID.  */
      gupcr_wait_le_count += 1;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_le_ct, gupcr_wait_le_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_le_eq);
	  gupcr_fatal_error ("received an error on wait LE");
	}
    }

  received_barrier_id = gupcr_notify_value;

  /* An inner thread sends the derived consensus
     minimum barrier ID to its children.  */
  if (!LEAF_THREAD)
    {
      int i;

      /* Re-initialize the barrier ID maximum range value.  */
      gupcr_notify_value = BARRIER_ID_MAX;

      /* Send the derived consensus minimum barrier ID to
         this thread's children.  */
      for (i = 0; i < gupcr_child_cnt; i++)
	{
	  rpid.rank = gupcr_child[i];
	  gupcr_portals_call (PtlPut,
			      (gupcr_wait_md, 0, BARRIER_ID_SIZE,
			       PTL_OC_ACK_REQ, rpid,
			       GUPCR_PTL_PTE_BARRIER_DOWN, PTL_NO_MATCH_BITS,
			       0, PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));
	}

      /* Wait until all children receive the consensus minimum
         barrier ID that is propagated down the tree.  */
      gupcr_wait_md_count += gupcr_child_cnt;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_md_ct, gupcr_wait_md_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_md_eq);
	  gupcr_fatal_error ("received an error on wait MD");
	}
    }

#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

  /* Verify that the barrier ID matches.  */
  if (barrier_id != INT_MIN &&
      barrier_id != received_barrier_id &&
      received_barrier_id != BARRIER_ID_MAX)
    gupcr_error ("thread %d: UPC barrier identifier mismatch among threads - "
		 "expected %d, received %d",
		 MYTHREAD, barrier_id, received_barrier_id);

  /* UPC Shared Memory Consistency Model requires all outstanding
     read/write operations to complete on the thread's enter
     into the next synchronization phase.  */
  gupcr_gmem_sync ();

  gupcr_barrier_active = 0;

  gupcr_trace (FC_BARRIER, "BARRIER WAIT EXIT %d", barrier_id);
}

/**
 * @fn __upc_barrier (int barrier_id)
 * UPC language upc_barrier implementation.
 *
 * @param [in] barrier_id Barrier ID
 */
void
__upc_barrier (int barrier_id)
{
  __upc_notify (barrier_id);
  __upc_wait (barrier_id);
}

/* This Portals4 based broadcast implementation uses barrier resources
 * to pass the broadcast message from thread 0 to all other threads.  */

/**
 * @fn gupcr_bcast_send (void *value, size_t nbytes)
 * Send broadcast message to all thread's children.
 *
 * The broadcast is a collective operation where thread 0 (root thread)
 * sends a message to all other threads.  This function must be
 * called by the thread 0 only from a public function
 * "gupcr_broadcast_put".
 *
 * @param [in] value Pointer to send value
 * @param [in] nbytes Number of bytes to send
 * @ingroup BROADCAST
 */
void
gupcr_bcast_send (void *value, size_t nbytes)
{
  int i;
  ptl_process_t rpid;
  ptl_ct_event_t ct;

  gupcr_trace (FC_BROADCAST, "BROADCAST SEND ENTER 0x%lx %lu",
	       (long unsigned) value, (long unsigned) nbytes);

  /* This broadcast operation is implemented a collective operation.
     Before proceeding, complete all outstanding shared memory
     read/write operations.  */
  gupcr_gmem_sync ();

  /* Copy the message into the buffer used for delivery
     to the children threads.  */
  memcpy (gupcr_wait_ptr, value, nbytes);

  gupcr_notify_le_count += gupcr_child_cnt;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_notify_le_ct, gupcr_notify_le_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_notify_le_eq);
      gupcr_fatal_error ("received an error on notify LE");
    }

  /* Send broadcast to this thread's children.  */
  for (i = 0; i < gupcr_child_cnt; i++)
    {
      rpid.rank = gupcr_child[i];
      gupcr_debug (FC_BROADCAST, "Send broadcast message to child (%d)",
		   gupcr_child[i]);
      gupcr_portals_call (PtlPut, (gupcr_wait_md, 0,
				   nbytes, PTL_ACK_REQ, rpid,
				   GUPCR_PTL_PTE_BARRIER_DOWN,
				   PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
				   PTL_NULL_HDR_DATA));
    }

  /* Wait for message delivery to all children.  This ensures that
     the source buffer is not overwritten by back-to-back
     broadcast operations.  */
  gupcr_wait_md_count += gupcr_child_cnt;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_wait_md_ct, gupcr_wait_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_wait_md_eq);
      gupcr_fatal_error ("received an error on wait MD");
    }
  gupcr_trace (FC_BROADCAST, "BROADCAST SEND EXIT");
}

/**
 * @fn gupcr_bcast_recv (void *value, size_t nbytes)
 * Wait to receive the broadcast message and return its value.
 *
 * Broadcast is a collective operation where thread 0 (the root thread)
 * sends a message to all other threads.  This function must be
 * called by every thread other then thread 0.
 *
 * @param [in] value Pointer to received value
 * @param [in] nbytes Number of bytes to receive
 * @ingroup BROADCAST
 */
void
gupcr_bcast_recv (void *value, size_t nbytes)
{
  int i;
  ptl_process_t rpid;
  ptl_ct_event_t ct;

  gupcr_trace (FC_BROADCAST, "BROADCAST RECV ENTER 0x%lx %lu",
	       (long unsigned) value, (long unsigned) nbytes);

  gupcr_gmem_sync ();

#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
  if (INNER_THREAD)
    {
      /* Prepare triggers for message push to all children.  */
      gupcr_wait_le_count += 1;
      for (i = 0; i < gupcr_child_cnt; i++)
	{
	  rpid.rank = gupcr_child[i];
	  gupcr_debug (FC_BROADCAST,
		       "Set broadcast trigger to the child (%d)",
		       gupcr_child[i]);
	  /* Trigger: message received from the parent.
	     Action: send the message to the child.  */
	  gupcr_portals_call (PtlTriggeredPut, (gupcr_wait_md, 0,
						nbytes, PTL_ACK_REQ, rpid,
						GUPCR_PTL_PTE_BARRIER_DOWN,
						PTL_NO_MATCH_BITS, 0,
						PTL_NULL_USER_PTR,
						PTL_NULL_HDR_DATA,
						gupcr_wait_le_ct,
						gupcr_wait_le_count));
	}

      /* Prepare a trigger to send notification to the parent.  */
      gupcr_debug (FC_BROADCAST,
		   "Set notification trigger to the parent (%d)",
		   gupcr_parent_thread);
      rpid.rank = gupcr_parent_thread;
      gupcr_barrier_value = BARRIER_ID_MAX;
      /* Trigger: notification received from all children.
         Action: send notification to the parent.  */
      gupcr_notify_le_count += gupcr_child_cnt;
      gupcr_portals_call (PtlTriggeredPut, (gupcr_barrier_md, 0,
					    BARRIER_ID_SIZE,
					    PTL_NO_ACK_REQ, rpid,
					    GUPCR_PTL_PTE_BARRIER_UP,
					    PTL_NO_MATCH_BITS, 0,
					    PTL_NULL_USER_PTR,
					    PTL_NULL_HDR_DATA,
					    gupcr_notify_le_ct,
					    gupcr_notify_le_count));

      /* Wait for delivery to all children.  */
      gupcr_wait_md_count += gupcr_child_cnt;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_md_ct, gupcr_wait_md_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_md_eq);
	  gupcr_fatal_error ("received an error on wait MD");
	}
      gupcr_debug (FC_BROADCAST, "Received PtlPut acks: %lu",
                   (long unsigned) ct.success);
    }
  else
    {
      /* A leaf thread sends notification to its parent that
         it is ready to receive the broadcast value.  */
      gupcr_debug (FC_BROADCAST, "Send notification to the parent (%d)",
		   gupcr_parent_thread);
      rpid.rank = gupcr_parent_thread;
      gupcr_barrier_value = BARRIER_ID_MAX;
      gupcr_portals_call (PtlPut, (gupcr_barrier_md, 0,
				   BARRIER_ID_SIZE, PTL_NO_ACK_REQ, rpid,
				   GUPCR_PTL_PTE_BARRIER_UP,
				   PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
				   PTL_NULL_HDR_DATA));

      /* Wait to receive a message from the parent.  */
      gupcr_wait_le_count += 1;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_wait_le_ct, gupcr_wait_le_count, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_le_eq);
	  gupcr_fatal_error ("received an error on wait LE");
	}
    }
  memcpy (value, gupcr_wait_ptr, nbytes);
#else
  /* Inner threads must wait for its children threads to arrive.  */
  if (INNER_THREAD)
    {
      gupcr_debug (FC_BROADCAST, "Waiting for %d notifications",
		   gupcr_child_cnt);
      gupcr_notify_le_count += gupcr_child_cnt;
      gupcr_portals_call (PtlCTWait,
			  (gupcr_notify_le_ct, gupcr_child_cnt, &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_notify_le_eq);
	  gupcr_fatal_error ("received an error on notify LE");
	}
      gupcr_debug (FC_BROADCAST, "Received %lu broadcast notifications",
		   (long unsigned) ct.success);
    }

  /* Inform the parent that this thread and all its children arrived.
     Send barrier MAX value as we share PTEs with the barrier
     implementation.  */
  gupcr_debug (FC_BROADCAST, "Send notification to the parent %d",
	       gupcr_parent_thread);
  rpid.rank = gupcr_parent_thread;
  gupcr_barrier_value = BARRIER_ID_MAX;
  gupcr_portals_call (PtlPut, (gupcr_barrier_md, 0,
			       BARRIER_ID_SIZE, PTL_NO_ACK_REQ, rpid,
			       GUPCR_PTL_PTE_BARRIER_UP, PTL_NO_MATCH_BITS, 0,
			       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));

  /* Receive the broadcast message from the parent.  */
  gupcr_wait_le_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_wait_le_ct, gupcr_wait_le_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_wait_le_eq);
      gupcr_fatal_error ("received an error on wait LE");
    }

  /* Copy the received message.  */
  memcpy (value, gupcr_wait_ptr, nbytes);

  if (INNER_THREAD)
    {
      /* An inner thread must pass the message to its children.  */
      for (i = 0; i < gupcr_child_cnt; i++)
	{
	  gupcr_debug (FC_BROADCAST, "Sending a message to %d",
		       gupcr_child[i]);
	  rpid.rank = gupcr_child[i];
	  gupcr_portals_call (PtlPut, (gupcr_wait_md, 0,
				       nbytes, PTL_ACK_REQ, rpid,
				       GUPCR_PTL_PTE_BARRIER_DOWN,
				       PTL_NO_MATCH_BITS, 0,
				       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));
	}
      /* Wait for delivery to all children.  */
      gupcr_wait_md_count += gupcr_child_cnt;
      gupcr_portals_call (PtlCTWait, (gupcr_wait_md_ct, gupcr_wait_md_count,
				      &ct));
      if (ct.failure)
	{
	  gupcr_process_fail_events (gupcr_wait_md_eq);
          gupcr_fatal_error ("received an error on wait MD");
	}
    }
#endif
  gupcr_trace (FC_BROADCAST, "BROADCAST RECV EXIT");
}

/**
 * @fn gupcr_barrier_init (void)
 * Initialize barrier resources.
 * @ingroup INIT
 */
void
gupcr_barrier_init (void)
{
  ptl_pt_index_t pte;
  ptl_le_t le;
  ptl_md_t md;

  gupcr_log (FC_BARRIER, "barrier init called");

  /* Create necessary CT handles.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_notify_le_ct));
  gupcr_notify_le_count = 0;
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_notify_md_ct));
  gupcr_notify_md_count = 0;
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_wait_le_ct));
  gupcr_wait_le_count = 0;
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_wait_md_ct));
  gupcr_wait_md_count = 0;
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_barrier_md_ct));
  gupcr_barrier_md_count = 0;
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_barrier_max_md_ct));
  gupcr_barrier_max_md_count = 0;

  /* Create necessary EQ handles.  Allocate only one event queue entry
     as we abort on any error.  */
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_notify_le_eq));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_notify_md_eq));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_wait_le_eq));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_wait_md_eq));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_barrier_md_eq));
  gupcr_portals_call (PtlEQAlloc,
		      (gupcr_ptl_ni, 1, &gupcr_barrier_max_md_eq));

  /* Allocate PTEs.  */
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   gupcr_notify_le_eq,
				   GUPCR_PTL_PTE_BARRIER_UP, &pte));
  if (pte != GUPCR_PTL_PTE_BARRIER_UP)
    gupcr_fatal_error ("cannot allocate GUPCR_PTL_PTE_BARRIER_UP PTE");
  gupcr_debug (FC_BARRIER, "Barrier UP PTE allocated: %d",
	       GUPCR_PTL_PTE_BARRIER_UP);
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   gupcr_wait_le_eq,
				   GUPCR_PTL_PTE_BARRIER_DOWN, &pte));
  if (pte != GUPCR_PTL_PTE_BARRIER_DOWN)
    gupcr_fatal_error ("cannot allocate GUPCR_PTL_PTE_BARRIER_DOWN PTE");
  gupcr_debug (FC_BARRIER, "Barrier DOWN PTE allocated: %d",
	       GUPCR_PTL_PTE_BARRIER_DOWN);

  /* Children perform atomic MIN on up_value,
     make sure we start with the maximum possible value.  */
  gupcr_notify_value = BARRIER_ID_MAX;

  /* Create LE for barrier ID value traveling up the tree.  */
  le.start = &gupcr_notify_value;
  le.length = sizeof (gupcr_notify_value);
  le.ct_handle = gupcr_notify_le_ct;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET |
    PTL_LE_EVENT_CT_COMM | PTL_LE_EVENT_SUCCESS_DISABLE |
    PTL_LE_EVENT_LINK_DISABLE;
  gupcr_portals_call (PtlLEAppend,
		      (gupcr_ptl_ni, GUPCR_PTL_PTE_BARRIER_UP, &le,
		       PTL_PRIORITY_LIST, NULL, &gupcr_notify_le));

  /* Create LE for barrier ID value traveling down the tree.
     Allocate enough space as barrier resources are
     used to also broadcast arbitrary values.  */
  gupcr_malloc (gupcr_wait_ptr, GUPCR_MAX_BROADCAST_SIZE);
  le.start = gupcr_wait_ptr;
  le.length = GUPCR_MAX_BROADCAST_SIZE;
  le.ct_handle = gupcr_wait_le_ct;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET |
    PTL_LE_EVENT_CT_COMM | PTL_LE_EVENT_SUCCESS_DISABLE |
    PTL_LE_EVENT_LINK_DISABLE;
  gupcr_portals_call (PtlLEAppend,
		      (gupcr_ptl_ni, GUPCR_PTL_PTE_BARRIER_DOWN, &le,
		       PTL_PRIORITY_LIST, NULL, &gupcr_wait_le));

  /* Create source MD for barrier ID values sent up the tree.  */
  md.start = &gupcr_notify_value;
  md.length = sizeof (gupcr_notify_value);
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_notify_md_eq;
  md.ct_handle = gupcr_notify_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_notify_md));

  /* Create source MD for barrier ID values sent down the tree.  */
  md.start = gupcr_wait_ptr;
  md.length = GUPCR_MAX_BROADCAST_SIZE;
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_wait_md_eq;
  md.ct_handle = gupcr_wait_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_wait_md));

  /* Create source MD for barrier ID values sent up the tree.  */
  md.start = &gupcr_barrier_value;
  md.length = sizeof (gupcr_barrier_value);
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_barrier_md_eq;
  md.ct_handle = gupcr_barrier_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_barrier_md));

  /* Create source MD that is used re-initialize the
     the consensus minimum barrier ID value to a maximum
     possible value.  */
  md.start = &gupcr_barrier_max_value;
  md.length = sizeof (gupcr_barrier_max_value);
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_barrier_max_md_eq;
  md.ct_handle = gupcr_barrier_max_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_barrier_max_md));
}

/**
 * @fn gupcr_barrier_fini (void)
 * Release barrier resources.
 * @ingroup INIT
 */
void
gupcr_barrier_fini (void)
{
  gupcr_log (FC_BARRIER, "barrier fini called");

#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
  /* Cancel any outstanding triggered operations.  */
  gupcr_portals_call (PtlCTCancelTriggered, (gupcr_wait_le_ct));
  gupcr_portals_call (PtlCTCancelTriggered, (gupcr_barrier_max_md_ct));
  gupcr_portals_call (PtlCTCancelTriggered, (gupcr_notify_le_ct));
  gupcr_portals_call (PtlCTCancelTriggered, (gupcr_wait_md_ct));
#endif

  /* Release MDs and their CTs.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_barrier_md));
  gupcr_portals_call (PtlCTFree, (gupcr_barrier_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_barrier_md_eq));
  gupcr_portals_call (PtlMDRelease, (gupcr_barrier_max_md));
  gupcr_portals_call (PtlCTFree, (gupcr_barrier_max_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_barrier_max_md_eq));
  gupcr_portals_call (PtlMDRelease, (gupcr_notify_md));
  gupcr_portals_call (PtlCTFree, (gupcr_notify_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_notify_md_eq));
  gupcr_portals_call (PtlMDRelease, (gupcr_wait_md));
  gupcr_portals_call (PtlCTFree, (gupcr_wait_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_wait_md_eq));

  /* Release LEs, their CTs, and PTEs.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_notify_le));
  gupcr_portals_call (PtlCTFree, (gupcr_notify_le_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_notify_le_eq));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_BARRIER_UP));

  gupcr_portals_call (PtlLEUnlink, (gupcr_wait_le));
  gupcr_portals_call (PtlCTFree, (gupcr_wait_le_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_wait_le_eq));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_BARRIER_DOWN));
}

/** @} */
