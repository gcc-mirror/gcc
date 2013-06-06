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
 * @file gupcr_portals.c
 * GUPC Portals4 Initialization.
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_runtime.h"

/* Portals network interface handle/limits/rank */
ptl_handle_ni_t gupcr_ptl_ni;
ptl_ni_limits_t gupcr_ptl_ni_limits;
ptl_rank_t gupcr_ptl_rank;
ptl_nid_t gupcr_ptl_nid;
ptl_pid_t gupcr_ptl_pid;
int gupcr_ptl_size;

int gupcr_child[GUPCR_TREE_FANOUT];
int gupcr_child_cnt;
int gupcr_parent_thread;

size_t gupcr_max_ordered_size;
size_t gupcr_max_msg_size;
size_t gupcr_max_volatile_size;

/** Mapping to nid/pid for each rank */
static ptl_process_t *gupcr_ptl_proc_map;

/**
 * @addtogroup PORTALS_RUNTIME GUPCR Portals runtime interface
 * @{
 */

/**
 * Return Portals error description string.
 *
 * @param [in] errnum Portals error number
 * @retval Error description string
 */
const char *
gupcr_strptlerror (int errnum)
{
  static char gupcr_strptlerror_buf[64];
  switch (errnum)
    {
    case PTL_OK:
      return "portals operation successful";
    case PTL_ARG_INVALID:
      return "invalid portals argument";
    case PTL_CT_NONE_REACHED:
      return "timeout reached";
    case PTL_EQ_DROPPED:
      return "portals event dropped";
    case PTL_EQ_EMPTY:
      return "portals event queue empty";
    case PTL_FAIL:
      return "portals operation failed";
    case PTL_IGNORED:
      return "logical map set failed";
    case PTL_IN_USE:
      return "portals resource already in use";
    case PTL_INTERRUPTED:
      return "portals operation interrupted";
    case PTL_LIST_TOO_LONG:
      return "portals list too long";
    case PTL_NO_INIT:
      return "portals not initialized";
    case PTL_NO_SPACE:
      return "portals out of memory";
    case PTL_PID_IN_USE:
      return "portals portal ID already in use";
    case PTL_PT_FULL:
      return "portals portal table is full";
    case PTL_PT_EQ_NEEDED:
      return "portals event queue needed for flow control";
    case PTL_PT_IN_USE:
      return "portals PTE already in use";
    default:
      break;
    }
  sprintf (gupcr_strptlerror_buf, "unknown portals status code: %d", errnum);
  return gupcr_strptlerror_buf;
}

/**
 * Return Event Queue type description string.
 *
 * @param [in] eqtype Event queue type
 * @retval Event queue type description string
 */
const char *
gupcr_streqtype (ptl_event_kind_t eqtype)
{
  switch (eqtype)
    {
    case PTL_EVENT_GET:
      return "PTL_EVENT_GET";
    case PTL_EVENT_GET_OVERFLOW:
      return "PTL_EVENT_GET_OVERFLOW";
    case PTL_EVENT_PUT:
      return "PTL_EVENT_PUT";
    case PTL_EVENT_PUT_OVERFLOW:
      return "PTL_EVENT_PUT_OVERFLOW";
    case PTL_EVENT_ATOMIC:
      return "PTL_EVENT_ATOMIC";
    case PTL_EVENT_ATOMIC_OVERFLOW:
      return "PTL_EVENT_ATOMIC_OVERFLOW";
    case PTL_EVENT_FETCH_ATOMIC:
      return "PTL_EVENT_ATOMIC";
    case PTL_EVENT_FETCH_ATOMIC_OVERFLOW:
      return "PTL_EVENT_ATOMIC_OVERFLOW";
    case PTL_EVENT_REPLY:
      return "PTL_EVENT_REPLY";
    case PTL_EVENT_SEND:
      return "PTL_EVENT_SEND";
    case PTL_EVENT_ACK:
      return "PTL_EVENT_ACK";
    case PTL_EVENT_PT_DISABLED:
      return "PTL_EVENT_PT_DISABLED";
    case PTL_EVENT_LINK:
      return "PTL_EVENT_LINK";
    case PTL_EVENT_AUTO_UNLINK:
      return "PTL_EVENT_AUTO_UNLINK";
    case PTL_EVENT_AUTO_FREE:
      return "PTL_EVENT_AUTO_FREE";
    case PTL_EVENT_SEARCH:
      return "PTL_EVENT_SEARCH";
    }
  return "UNKNOWN EVENT TYPE";
}

/**
 * Return Data type description string.
 *
 * @param [in] datatype Data type
 * @retval Data type description string
 */
const char *
gupcr_strptldatatype (ptl_datatype_t datatype)
{
  switch (datatype)
    {
    case PTL_INT8_T:
      return "PTL_INT8_T";
    case PTL_UINT8_T:
      return "PTL_UINT8_T";
    case PTL_INT16_T:
      return "PTL_INT16_T";
    case PTL_UINT16_T:
      return "PTL_UINT16_T";
    case PTL_INT32_T:
      return "PTL_INT32_T";
    case PTL_UINT32_T:
      return "PTL_UINT32_T";
    case PTL_FLOAT:
      return "PTL_FLOAT";
    case PTL_INT64_T:
      return "PTL_INT64_T";
    case PTL_UINT64_T:
      return "PTL_UINT64_T";
    case PTL_DOUBLE:
      return "PTL_DOUBLE";
    case PTL_FLOAT_COMPLEX:
      return "PTL_FLOAT_COMPLEX";
    case PTL_DOUBLE_COMPLEX:
      return "PTL_DOUBLE_COMPLEX";
    case PTL_LONG_DOUBLE:
      return "PTL_LONG_DOUBLE";
    case PTL_LONG_DOUBLE_COMPLEX:
      return "PTL_LONG_DOUBLE_COMPLEX";
    }
  return "UNKNOWN DATA TYPE";
}

/**
 * Return Atomic operation description string.
 *
 * @param [in] op Atomic operation type
 * @retval Atomic operation description string
 */
const char *
gupcr_strptlop (ptl_op_t op)
{
  switch (op)
    {
    case PTL_MIN:
      return "PTL_MIN";
    case PTL_MAX:
      return "PTL_MAX";
    case PTL_SUM:
      return "PTL_SUM";
    case PTL_PROD:
      return "PTL_PROD";
    case PTL_LOR:
      return "PTL_LOR";
    case PTL_LAND:
      return "PTL_LAND";
    case PTL_BOR:
      return "PTL_BOR";
    case PTL_BAND:
      return "PTL_BAND";
    case PTL_LXOR:
      return "PTL_LXOR";
    case PTL_BXOR:
      return "PTL_BXOR";
    case PTL_SWAP:
      return "PTL_SWAP";
    case PTL_CSWAP:
      return "PTL_CSWAP";
    case PTL_CSWAP_NE:
      return "PTL_CSWAP_NE";
    case PTL_CSWAP_LE:
      return "PTL_CSWAP_LE";
    case PTL_CSWAP_LT:
      return "PTL_CSWAP_LT";
    case PTL_CSWAP_GE:
      return "PTL_CSWAP_GE";
    case PTL_CSWAP_GT:
      return "PTL_CSWAP_GT";
    case PTL_MSWAP:
      return "PTL_MSWAP";
    }
  return "UNKNOWN ATOMIC OPERATION TYPE";
}

/**
 * Return Network Interface error description string.
 *
 * @param [in] nitype NI failure type
 * @retval NI failure description string.
 */
const char *
gupcr_nifailtype (ptl_ni_fail_t nitype)
{
  switch (nitype)
    {
    case PTL_NI_OK:
      return "PTL_NI_OK";
    case PTL_NI_UNDELIVERABLE:
      return "PTL_NI_UNDELIVERABLE";
    case PTL_NI_DROPPED:
      return "PTL_NI_DROPPED";
    case PTL_NI_PT_DISABLED:
      return "PTL_NI_PT_DISABLED";
    case PTL_NI_PERM_VIOLATION:
      return "PTL_NI_PERM_VIOLATION";
    case PTL_NI_OP_VIOLATION:
      return "PTL_NI_OP_VIOLATION";
    case PTL_NI_NO_MATCH:
      return "PTL_NI_NO_MATCH";
    case PTL_NI_SEGV:
      return "PTL_NI_SEGV";
    }
  return "NI_FAILURE_UNKNOWN";
}

/**
 * Return Portals atomic data type from the specified size.
 */
ptl_datatype_t
gupcr_get_atomic_datatype (int size)
{
  switch (size)
    {
    case 1:
      return PTL_UINT8_T;
    case 2:
      return PTL_UINT16_T;
    case 4:
      return PTL_UINT32_T;
    case 8:
      return PTL_UINT64_T;
    case 16:
      return PTL_DOUBLE_COMPLEX;
    default:
      gupcr_fatal_error
	("Unable to convert size of %d into Portals atomic data type.", size);
    }
  return -1;
}

/**
 * Return Portals data size from the specified atomic type.
 *
 * @param [in] type Portals atomic data type
 * @retval Portals atomic data type size
 */
size_t
gupcr_get_atomic_size (ptl_datatype_t type)
{
  switch (type)
    {
    case PTL_INT8_T:
    case PTL_UINT8_T:
      return 1;
    case PTL_INT16_T:
    case PTL_UINT16_T:
      return 2;
    case PTL_INT32_T:
    case PTL_UINT32_T:
      return 4;
    case PTL_INT64_T:
    case PTL_UINT64_T:
      return 8;
    case PTL_FLOAT:
      return __SIZEOF_FLOAT__;
    case PTL_FLOAT_COMPLEX:
      return 2 * __SIZEOF_FLOAT__;
    case PTL_DOUBLE:
      return __SIZEOF_DOUBLE__;
    case PTL_DOUBLE_COMPLEX:
      return 2 * __SIZEOF_DOUBLE__;
#ifdef __SIZEOF_LONG_DOUBLE__
    case PTL_LONG_DOUBLE:
      return __SIZEOF_LONG_DOUBLE__;
    case PTL_LONG_DOUBLE_COMPLEX:
      return 2 * __SIZEOF_LONG_DOUBLE__;
#endif
    default:
      gupcr_fatal_error
	("Unknown atomic type %d", (int) type);
    }
  return -1;
}

/**
 * @fn gupcr_process_fail_events (ptl_handle_eq_t eq)
 * Show information on failed events.
 *
 * This procedure prints the contents of the event queue.  As barrier
 * implementation does not use full events, the event queue contains
 * only failure events.  This procedure is called only if any of the
 * counting events reported a failure.
 *
 * @param [in] eq Event Queue ID
 */
void
gupcr_process_fail_events (ptl_handle_eq_t eq)
{
  ptl_event_t ev;
  int status;

  while ((status = PtlEQGet (eq, &ev)) != PTL_EQ_EMPTY)
    {
      const char *eqerr = gupcr_streqtype (ev.type);
      const char *nierr = gupcr_nifailtype (ev.ni_fail_type);
      gupcr_error_print ("Event failure %s (%s (%x))", eqerr, nierr,
			 ev.ni_fail_type);
    }
}

/**
 * Get current thread rank.
 * @retval Rank of the current thread
 */
int
gupcr_get_rank (void)
{
  return gupcr_ptl_rank;
}

/**
 * Get number of running threads.
 * @retval Number of running threads
 */
int
gupcr_get_threads_count (void)
{
  return gupcr_ptl_size;
}

/**
 * Get process PID for specified rank.
 * @param [in] rank Rank of the thread
 * @retval PID of the thread
 */
int
gupcr_get_rank_pid (int rank)
{
  return gupcr_ptl_proc_map[rank].phys.pid;
}

/**
 * Get process NID for specified rank.
 * @param [in] rank Rank of the thread
 * @retval NID of the thread
 */
int
gupcr_get_rank_nid (int rank)
{
  return gupcr_ptl_proc_map[rank].phys.nid;
}

/**
 * Wait for all threads to complete initialization.
 */
void
gupcr_startup_barrier (void)
{
  gupcr_runtime_barrier ();
}

/** @} */

/**
 * @addtogroup INIT GUPCR Initialization
 * @{
 */

/**
 * Initialize Portals Interface.
 *
 * Calls to get rank/number of threads can
 * be made only after Portals is initialized.
 * Also, initialize the necessary runtime.
 */
void
gupcr_portals_init (void)
{
  gupcr_portals_call (PtlInit, ());
  /* Set rank for this thread.  */
  gupcr_ptl_rank = gupcr_runtime_get_rank ();
  gupcr_ptl_size = gupcr_runtime_get_size ();
}

/**
 * Close Portals.
 */
void
gupcr_portals_fini (void)
{
  PtlFini ();
}

/**
 * Initialize Portals Networking Interface.
 */
void
gupcr_portals_ni_init (void)
{
  /* Initialize network interface.  */
  gupcr_portals_call (PtlNIInit,
		      (PTL_IFACE_DEFAULT, PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
		       PTL_PID_ANY, NULL, &gupcr_ptl_ni_limits,
		       &gupcr_ptl_ni));
  /* Initialize limits used by GMEM.  */
  gupcr_max_ordered_size = gupcr_ptl_ni_limits.max_waw_ordered_size;
  gupcr_max_msg_size = gupcr_ptl_ni_limits.max_msg_size;
  gupcr_max_volatile_size = gupcr_ptl_ni_limits.max_volatile_size;

  /* Initialize the mapping from rank -> nid/pid.  */
  gupcr_ptl_proc_map = gupcr_runtime_get_mapping (gupcr_ptl_ni);
  gupcr_portals_call (PtlSetMap, (gupcr_ptl_ni, (ptl_size_t) gupcr_ptl_size,
				  gupcr_ptl_proc_map));

  /* Get this thread physical IDs.  */
  gupcr_ptl_pid = gupcr_get_rank_pid (gupcr_ptl_rank);
  gupcr_ptl_nid = gupcr_get_rank_nid (gupcr_ptl_rank);
}

/**
 * Close Portals Networking Interface.
 */
void
gupcr_portals_ni_fini (void)
{
  gupcr_portals_call (PtlNIFini, (gupcr_ptl_ni));
}

/**
 * Find the node's parent and all its children.
 */
void
gupcr_nodetree_setup (void)
{
  int i;
  gupcr_log ((FC_BARRIER | FC_BROADCAST),
	     "node tree initialized with fanout of %d", GUPCR_TREE_FANOUT);
  for (i = 0; i < GUPCR_TREE_FANOUT; i++)
    {
      int child = GUPCR_TREE_FANOUT * MYTHREAD + i + 1;
      if (child < THREADS)
	{
	  gupcr_child_cnt++;
	  gupcr_child[i] = child;
	}
    }
  if (MYTHREAD == 0)
    gupcr_parent_thread = ROOT_PARENT;
  else
    gupcr_parent_thread = (MYTHREAD - 1) / GUPCR_TREE_FANOUT;
}

/** @} */
