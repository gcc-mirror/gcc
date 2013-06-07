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

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_lib.h"
#include "gupcr_lock_sup.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_lock_sup.h"

/**
 * @file gupcr_lock_sup.c
 * GUPC Portals4 locks implementation support routines.
 *
 * @addtogroup LOCK GUPCR Lock Functions
 * @{
 */

/** Lock shared access LE handle */
static ptl_handle_le_t gupcr_lock_le;
/** Lock shared access LE counting events handle */
static ptl_handle_ct_t gupcr_lock_le_ct;
/** Lock shared access LE counting events counter */
static ptl_size_t gupcr_lock_le_count;
/** Lock shared access LE events queue handle */
static ptl_handle_eq_t gupcr_lock_le_eq;

/** Lock buffer for CSWAP operation */
static char gupcr_lock_buf[16];
/** Lock shared access MD handle */
static ptl_handle_md_t gupcr_lock_md;
/** Lock shared access MD counting events handle */
static ptl_handle_ct_t gupcr_lock_md_ct;
/** Lock shared access MD counting events counter */
static ptl_size_t gupcr_lock_md_count;
/** Lock shared access MD event queue handle */
static ptl_handle_eq_t gupcr_lock_md_eq;

/**
 * Execute lock-related atomic fetch and store remote operation.
 *
 * Value "val" is written into the specified remote location and the
 * old value is returned.
 *
 * A Portals 'swap atomic' operation is used when the acquiring thread must
 * atomically determine if the lock is available.  A pointer to the thread's
 * local lock waiting list link is atomically written into the lock's 'last'
 * field, and the current value of the 'last' field is returned.  If NULL,
 * the acquiring thread is the new owner, otherwise it must insert itself
 * onto the waiting list.
 */
void
gupcr_lock_swap (size_t dest_thread,
		 size_t dest_offset, void *val, void *old, size_t size)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  gupcr_debug (FC_LOCK, "%lu:0x%lx",
                        (long unsigned) dest_thread,
                        (long unsigned) dest_offset);
  rpid.rank = dest_thread;
  gupcr_portals_call (PtlSwap, (gupcr_lock_md, (ptl_size_t) old,
				gupcr_lock_md, (ptl_size_t) val, size, rpid,
				GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				dest_offset, PTL_NULL_USER_PTR, 0, NULL,
				PTL_SWAP, gupcr_get_atomic_datatype (size)));
  gupcr_lock_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_lock_md_ct, gupcr_lock_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_lock_md_eq);
      gupcr_fatal_error ("received an error on lock MD");
    }
}

/**
 * Execute a lock-related atomic compare and swap operation.
 *
 * The value  pointed to by 'val' is written into the remote location
 * given by ('dest_thread', 'dest_addr) only if value in the destination
 * is identical to 'cmp'.
 *
 * A Portals compare and swap atomic operation is used during the lock
 * release phase when the owner of the lock must atomically determine if
 * there are threads waiting on the lock.  This is accomplished by using
 * the Portals CSWAP atomic operation, where a NULL pointer is written
 * into the lock's 'last' field only if this field contains the pointer
 * to the owner's local lock link structure.
 *
 * @retval Return TRUE if the operation was successful.
 */
int
gupcr_lock_cswap (size_t dest_thread,
		  size_t dest_offset, void *cmp, void *val, size_t size)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  gupcr_debug (FC_LOCK, "%lu:0x%lx",
                        (long unsigned) dest_thread,
			(long unsigned) dest_offset);
  rpid.rank = dest_thread;
  gupcr_portals_call (PtlSwap, (gupcr_lock_md, (ptl_size_t) gupcr_lock_buf,
				gupcr_lock_md, (ptl_size_t) val, size, rpid,
				GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				dest_offset, PTL_NULL_USER_PTR, 0, cmp,
				PTL_CSWAP, gupcr_get_atomic_datatype (size)));
  gupcr_lock_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_lock_md_ct, gupcr_lock_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_lock_md_eq);
      gupcr_fatal_error ("received an error on lock MD");
    }
  return !memcmp (cmp, gupcr_lock_buf, size);
}

/*
 * Execute a Portals put operation on the lock-related PTE.
 *
 * Execute a put operation on the PTE that is reserved for
 * lock-related operations (PTL_PTE_UPC_LOCK).  This separate PTE is used
 * to make it possible to count only Portals put operations on the
 * 'signal' or 'next' words of a UPC lock wait list entry.
 *
 * gupcr_lock_put() is used to 'signal' the remote thread that:
 * - ownership of the lock is passed to a remote thread if the remote
 * thread is the next thread on the waiting list
 * - a pointer to the calling thread's local control block has
 * been appended to the lock's waiting list
 */
void
gupcr_lock_put (size_t dest_thread, size_t dest_addr, void *val, size_t size)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  gupcr_debug (FC_LOCK, "%lu:0x%lx",
                        (long unsigned) dest_thread,
			(long unsigned) dest_addr);
  rpid.rank = dest_thread;
  gupcr_portals_call (PtlPut, (gupcr_lock_md, (ptl_size_t) val,
			       size, PTL_ACK_REQ, rpid,
			       GUPCR_PTL_PTE_LOCK, PTL_NO_MATCH_BITS,
			       (ptl_size_t) dest_addr,
			       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));
  gupcr_lock_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_lock_md_ct, gupcr_lock_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_lock_md_eq);
      gupcr_fatal_error ("received an error on lock MD");
    }
}

/*
 * Execute a Portals get operation on the lock-related PTE.
 *
 * All operations on lock/link data structures must be performed
 * through the Portals interface to prevent data tearing.
 */
void
gupcr_lock_get (size_t dest_thread, size_t dest_addr, void *val, size_t size)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  gupcr_debug (FC_LOCK, "%lu:0x%lx",
                        (long unsigned) dest_thread,
			(long unsigned) dest_addr);
  rpid.rank = dest_thread;
  gupcr_portals_call (PtlGet, (gupcr_lock_md, (ptl_size_t) val,
			       size, rpid,
			       GUPCR_PTL_PTE_LOCK, PTL_NO_MATCH_BITS,
			       (ptl_size_t) dest_addr,
			       PTL_NULL_USER_PTR));
  gupcr_lock_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_lock_md_ct, gupcr_lock_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_lock_md_eq);
      gupcr_fatal_error ("received an error on lock MD");
    }
}

/**
 * Wait for the next counting event to be posted to lock LE.
 *
 * This function is called when it has been determined that
 * the current thread needs to wait until the lock is is released.
 *
 * Wait until the next Portals counting event is posted
 * to the LE reserved for this purpose and then return.
 * The caller will check whether the lock was in fact released,
 * and if not, will call this function again to wait for the
 * next lock-related event to come in.
 */
void
gupcr_lock_wait (void)
{
  ptl_ct_event_t ct;
  gupcr_debug (FC_LOCK, "");
  gupcr_lock_le_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_lock_le_ct, gupcr_lock_le_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_lock_le_eq);
      gupcr_fatal_error ("received an error on lock LE");
    }
}

/**
 * Initialize lock resources.
 * @ingroup INIT
 */
void
gupcr_lock_init (void)
{
  ptl_md_t md;
  ptl_pt_index_t pte;
  ptl_le_t le;
  gupcr_log (FC_LOCK, "lock init called");
  /* Allocate Portals PTE for locks.  */
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_lock_le_eq));
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   gupcr_lock_le_eq, GUPCR_PTL_PTE_LOCK,
				   &pte));
  if (pte != GUPCR_PTL_PTE_LOCK)
    gupcr_fatal_error ("cannot allocate PTE GUPCR_PTL_PTE_LOCK.");
  gupcr_debug (FC_LOCK, "Lock PTE allocated: %d", GUPCR_PTL_PTE_LOCK);
  /* Allocate LE for locks.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_lock_le_ct));
  gupcr_lock_le_count = 0;
  le.start = gupcr_gmem_base;
  le.length = gupcr_gmem_size;
  le.ct_handle = gupcr_lock_le_ct;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | PTL_LE_EVENT_CT_COMM |
    PTL_LE_EVENT_SUCCESS_DISABLE | PTL_LE_EVENT_LINK_DISABLE;
  gupcr_portals_call (PtlLEAppend, (gupcr_ptl_ni, GUPCR_PTL_PTE_LOCK, &le,
				    PTL_PRIORITY_LIST, NULL, &gupcr_lock_le));
  gupcr_debug (FC_LOCK, "Lock LE created at 0x%lx with size 0x%lx)",
	       (long unsigned) gupcr_gmem_base,
	       (long unsigned) gupcr_gmem_size);
  /* Setup MD for writes into lock data structures located on
     other threads.  Map the entire user address space,
     though the MD probably could be constrained to the area where
     lock data structures are managed.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_lock_md_ct));
  gupcr_lock_md_count = 0;
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_lock_md_eq));
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options =
    PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_CT_REPLY |
    PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_lock_md_eq;
  md.ct_handle = gupcr_lock_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_lock_md));
  /* Initialize the lock link allocator.  */
  gupcr_lock_link_init ();
  /* Initialize the lock free list.  */
  gupcr_lock_free_init ();
  /* Initialize the heap allocator locks.  */
  gupcr_lock_heap_sup_init ();
}

/**
 * Release lock resources.
 * @ingroup INIT
 */
void
gupcr_lock_fini (void)
{
  gupcr_log (FC_LOCK, "lock fini called");
  /* Release lock MD.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_lock_md));
  gupcr_portals_call (PtlCTFree, (gupcr_lock_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_lock_md_eq));
  /* Release lock LE and PTE.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_lock_le));
  gupcr_portals_call (PtlCTFree, (gupcr_lock_le_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_lock_le_eq));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_LOCK));
}

/** @} */
