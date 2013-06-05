/* Copyright (C) 2012-2013
   Free Software Foundation, Inc.
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

#ifndef _GUPCR_PORTALS_H_
#define _GUPCR_PORTALS_H_

#include <portals4.h>

/**
 * @file gupcr_portals.h
 * GUPC Portals4 Global Definitions.
 */

/**
 * @addtogroup CONFIG GUPCR Configuration
 * @{
 */

/* GUPCR Portals Table Entries */
/** Memory put/get functions PTE */
#define	GUPCR_PTL_PTE_GMEM		GUPCR_PTE_BASE
/** Barrier messages to parent node PTE */
#define	GUPCR_PTL_PTE_BARRIER_UP	GUPCR_PTE_BASE+1
/** Barrier messages from parent node PTE */
#define	GUPCR_PTL_PTE_BARRIER_DOWN	GUPCR_PTE_BASE+2
/** Lock signaling PTE */
#define	GUPCR_PTL_PTE_LOCK		GUPCR_PTE_BASE+3
/** Shutdown service signaling PTE */
#define	GUPCR_PTL_PTE_SHUTDOWN		GUPCR_PTE_BASE+4
/** Collectives service signaling PTE */
#define	GUPCR_PTL_PTE_COLL		GUPCR_PTE_BASE+5
/** @} */

//begin lib_portals

/** Max ordered size - per network interface */
extern size_t gupcr_max_ordered_size;
#define GUPCR_PORTALS_MAX_ORDERED_SIZE gupcr_max_ordered_size
/** Max size of a message (put, get, or reply) */
extern size_t gupcr_max_msg_size;
#define GUPCR_PORTALS_MAX_MSG_SIZE gupcr_max_msg_size
/** Max size of a message that can use volatile memory descriptor */
extern size_t gupcr_max_volatile_size;
#define GUPCR_PORTALS_MAX_VOLATILE_SIZE gupcr_max_volatile_size

//end lib_portals

/** NULL value for matching bits */
#define PTL_NO_MATCH_BITS ((ptl_match_bits_t) 0)
/** NULL value for user pointer */
#define PTL_NULL_USER_PTR ((void *) 0)
/** NULL value for header data */
#define PTL_NULL_HDR_DATA ((ptl_hdr_data_t) 0)
/** Execute portals call and abort if error */
#define gupcr_portals_call(portals_func, args)				\
    do									\
      {									\
        int pstatus;							\
        pstatus = portals_func args;					\
	if (pstatus != PTL_OK)						\
	  gupcr_fatal_error ("UPC runtime Portals call "		\
	                     "`%s' on thread %d failed: %s\n", 		\
			     __STRING(portals_func), gupcr_get_rank (),	\
	                     gupcr_strptlerror (pstatus));		\
      }									\
    while (0)

/** Execute portals call and return status if there is no fatal error */
#define gupcr_portals_call_with_status(portals_func, pstatus, args)	\
    do									\
      {									\
        pstatus = portals_func args;					\
	if ((pstatus != PTL_OK) &&					\
	    (pstatus != PTL_CT_NONE_REACHED) &&				\
	    (pstatus != PTL_IN_USE) &&					\
	    (pstatus != PTL_INTERRUPTED))		        	\
	  gupcr_fatal_error ("UPC runtime Portals call "		\
	                     "`%s' on thread %d failed: %s\n",		\
			     __STRING(portals_func), gupcr_get_rank (),	\
	                     gupcr_strptlerror (pstatus));		\
      }									\
    while (0)

/**
 * @addtogroup GLOBAL GUPCR Global Variables
 * @{
 */

/** Network Interface Handle */
extern ptl_handle_ni_t gupcr_ptl_ni;
/** Network Interface Limits */
extern ptl_ni_limits_t gupcr_ptl_ni_limits;
/** Thread's process info */
extern ptl_process_t gupcr_ptl_myproc;
/** Thread's rank */
extern ptl_rank_t gupcr_ptl_rank;
/** Thread's NID */
extern ptl_nid_t gupcr_ptl_nid;
/** Thread's PID */
extern ptl_pid_t gupcr_ptl_pid;

/* For the purposes of implementing GUPC barriers, all UPC threads
   in a given job are organized as a tree.  Thread 0 is the
   root node (at the top of the tree).  Other threads can represent
   either an inner node (has at least one child), or a leaf
   node (has no children).  */

/** Parent of the root thread */
#define ROOT_PARENT -1

/** Thread IDs of all children of the current thread.  */
extern int gupcr_child[GUPCR_TREE_FANOUT];
/** Number of children of the current thread.  */
extern int gupcr_child_cnt;
/** Parent thread ID of the current thread.
    The tree root thread has a parent ID of -1.  */
extern int gupcr_parent_thread;

/** @} */

extern const char *gupcr_strptlerror (int);
extern const char *gupcr_streqtype (ptl_event_kind_t);
extern const char *gupcr_strptlop (ptl_op_t);
extern const char *gupcr_strptldatatype (ptl_datatype_t);
extern const char *gupcr_nifailtype (ptl_ni_fail_t);
extern void gupcr_process_fail_events (ptl_handle_eq_t);
extern ptl_datatype_t gupcr_get_atomic_datatype (int);
extern size_t gupcr_get_atomic_size (ptl_datatype_t);
extern int gupcr_get_rank (void);
extern int gupcr_get_threads_count (void);
extern int gupcr_get_rank_pid (int rank);
extern int gupcr_get_rank_nid (int rank);
extern void gupcr_startup_barrier (void);

extern void gupcr_portals_init (void);
extern void gupcr_portals_fini (void);
extern void gupcr_portals_ni_init (void);
extern void gupcr_portals_ni_fini (void);
extern void gupcr_nodetree_setup (void);

#endif /* gupcr_portals.h */
