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
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_coll_sup.h"

/**
 * @file gupcr_coll_sup.c
 * GUPC Portals4 collectives implementation support routines.
 *
 * @addtogroup COLLECTIVES GUPCR Collectives Functions
 * @{
 */

/** Collectives shared access LE handle */
static ptl_handle_le_t gupcr_coll_le;
/** Collectives shared access LE counting events handle */
static ptl_handle_ct_t gupcr_coll_le_ct;
/** Collectives shared access LE events queue handle */
static ptl_handle_ct_t gupcr_coll_le_eq;
/** Collectives number of received signals (PUT/ATOMIC) through LE */
static ptl_size_t gupcr_coll_signal_cnt;

/** Collectives local access MD handle */
static ptl_handle_md_t gupcr_coll_md;
/** Collectives local access MD counting events handle */
static ptl_handle_ct_t gupcr_coll_md_ct;
/** Collectives local access MD event queue handle */
static ptl_handle_ct_t gupcr_coll_md_eq;
/** Collectives number of received ACKs on local md */
static ptl_size_t gupcr_coll_ack_cnt;

/* Collectives thread tree.  */
/** Collectives tree parent thread */
int gupcr_coll_parent_thread;
/** Collectives tree number of children */
int gupcr_coll_child_cnt;
/** Collectives tree child's index */
int gupcr_coll_child_index;
/** Collectives tree children threads */
int gupcr_coll_child[GUPCR_TREE_FANOUT];

/**
 * Initialize collectives thread tree.
 *
 * A collectives tree starts from the "start" thread number and
 * includes only "nthreads" (e.g. threads involved in
 * the reduce process).  The simplest case is when all the
 * threads are involved in which case start=0 and
 * nthreads=THREADS (e.g. used for broadcast).
 *
 * The collectives thread tree can be organized in a
 * form where the "newroot" value identitifies
 * the root thread (only if the "newroot" thread
 * is participating in the operation).
 * @param [in] newroot A hint for the tree root thread.
 * @param [in] start Start thread for reduce
 * @param [in] nthreads Number of threads participating
 *
 */
void
gupcr_coll_tree_setup (size_t newroot, size_t start, int nthreads)
{
/* Convert from/to 0-(THREADS-1) to start-(nthreads-1) range.  */
#define NEWID(id,first) ((id - first + THREADS) % THREADS)
#define OLDID(nid,first) ((nid + first) % THREADS)

/* Remap into the new root (from root 0 to "root").  */
#define NEWIDROOT(id,top,cnt) ((cnt + id - top) % cnt)
#define OLDIDROOT(nid,top,cnt) ((nid + top) % cnt)
  int i;
  int ok_to_root = 0;
  int myid;
  int root = NEWID (newroot, start);

  gupcr_debug (FC_COLL, "newroot: %lu, start: %lu nthreads: %d",
	       (long unsigned) newroot, (long unsigned) start, nthreads);

  /* Check if root node is participating.  If yes, use that for the
     root, otherwise 0.  */
  if (root < nthreads)
    ok_to_root = 1;

  /* Get myid - first convert into the new range (0-nthreads),
     then, if needed and possible, into the range where newroot becomes 0.  */
  myid = NEWID (MYTHREAD, start);
  if (ok_to_root)
    myid = NEWIDROOT (myid, root, nthreads);

  /* Calculate the thread id's of the children and parent.  */
  gupcr_coll_child_cnt = 0;
  for (i = 0; i < GUPCR_TREE_FANOUT; i++)
    {
      int child = (GUPCR_TREE_FANOUT * myid + i + 1);
      if (child < nthreads)
	{
	  ++gupcr_coll_child_cnt;
	  if (ok_to_root)
	    child = OLDIDROOT (child, root, nthreads);
	  gupcr_coll_child[i] = OLDID (child, start);
	}
    }
  if (myid)
    {
      gupcr_coll_parent_thread = (myid - 1) / GUPCR_TREE_FANOUT;
      gupcr_coll_child_index =
	myid - gupcr_coll_parent_thread * GUPCR_TREE_FANOUT - 1;
      if (ok_to_root)
	gupcr_coll_parent_thread =
	  OLDIDROOT (gupcr_coll_parent_thread, root, nthreads);
      gupcr_coll_parent_thread = OLDID (gupcr_coll_parent_thread, start);
    }
  else
    gupcr_coll_parent_thread = ROOT_PARENT;
}

/**
 * Collective PUT operation
 *
 * @param [in] dthread Destination thread
 * @param [in] doffset Destination offset in the shared space
 * @param [in] soffset Source offset in the shared space
 * @param [in] nbytes Number of bytes to copy
 */

void
gupcr_coll_put (size_t dthread, size_t doffset, size_t soffset, size_t nbytes)
{
  ptl_process_t rpid;

  gupcr_debug (FC_COLL, "%d:0x%lx %lu:0x%lx %lu",
	       MYTHREAD, (long unsigned) soffset,
	       (long unsigned) dthread, (long unsigned) doffset,
	       (long unsigned) nbytes);
  rpid.rank = dthread;
  gupcr_portals_call (PtlPut,
		      (gupcr_coll_md, soffset, nbytes, PTL_ACK_REQ, rpid,
		       GUPCR_PTL_PTE_COLL, PTL_NO_MATCH_BITS, doffset,
		       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));
}

/**
 * Collective triggered PUT operation
 *
 * Schedule put operation once number of signals reaches
 * the specified value.
 *
 * @param [in] dthread Destination thread
 * @param [in] doffset Destination offset in the shared space
 * @param [in] soffset Source offset in the shared space
 * @param [in] nbytes Number of bytes to copy
 * @param [in] cnt Trigger count
 */
void
gupcr_coll_trigput (size_t dthread, size_t doffset, size_t soffset,
		    size_t nbytes, size_t cnt)
{
  ptl_process_t rpid;

  gupcr_debug (FC_COLL, "%d:0x%lx -> %lu:0x%lx %lu trig %lu",
	       MYTHREAD, (long unsigned) soffset,
	       (long unsigned) dthread, (long unsigned) doffset,
	       (long unsigned) nbytes, (long unsigned) cnt);
  rpid.rank = dthread;
  gupcr_portals_call (PtlTriggeredPut,
		      (gupcr_coll_md, soffset, nbytes, PTL_ACK_REQ, rpid,
		       GUPCR_PTL_PTE_COLL, PTL_NO_MATCH_BITS, doffset,
		       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA, gupcr_coll_le_ct,
		       gupcr_coll_signal_cnt + cnt));
}

/**
 * Collective atomic PUT operation.
 *
 * @param [in] dthread Destination thread
 * @param [in] doffset Destination offset in the shared space
 * @param [in] soffset Source offset in the shared space
 * @param [in] nbytes Number of bytes to copy
 * @param [in] op Portals atomic operation
 * @param [in] datatype Portals atomic data type
 */

void
gupcr_coll_put_atomic (size_t dthread, size_t doffset, size_t soffset,
		       size_t nbytes, ptl_op_t op, ptl_datatype_t datatype)
{
  ptl_process_t rpid;

  gupcr_debug (FC_COLL, "%d:0x%lx %lu:0x%lx %lu %s %s",
	       MYTHREAD, (long unsigned) soffset,
	       (long unsigned) dthread, (long unsigned) doffset,
	       (long unsigned) nbytes,
	       gupcr_strptlop (op), gupcr_strptldatatype (datatype));
  rpid.rank = dthread;
  gupcr_portals_call (PtlAtomic,
		      (gupcr_coll_md, soffset, nbytes, PTL_ACK_REQ, rpid,
		       GUPCR_PTL_PTE_COLL, PTL_NO_MATCH_BITS, doffset,
		       PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA, op, datatype));
}

/**
 * Collective triggered atomic PUT operation.
 *
 * Schedule atomic put operation once number of signals reaches
 * the specified value.
 *
 * @param [in] dthread Destination thread
 * @param [in] doffset Destination offset in the shared space
 * @param [in] soffset Source offset in the shared space
 * @param [in] nbytes Number of bytes to copy
 * @param [in] op Portals atomic operation
 * @param [in] datatype Portals atomic data type
 * @param [in] cnt Number of signals that triggers
 */
void
gupcr_coll_trigput_atomic (size_t dthread, size_t doffset, size_t soffset,
			   size_t nbytes, ptl_op_t op,
			   ptl_datatype_t datatype, size_t cnt)
{
  ptl_process_t rpid;

  gupcr_debug (FC_COLL, "%d:0x%lx %lu:0x%lx %lu %s %s trig %lu",
	       MYTHREAD, (long unsigned) soffset,
	       (long unsigned) dthread, (long unsigned) doffset,
	       (long unsigned) nbytes,
	       gupcr_strptlop (op), gupcr_strptldatatype (datatype),
	       (long unsigned) cnt);
  rpid.rank = dthread;
  gupcr_portals_call (PtlTriggeredAtomic,
		      (gupcr_coll_md, soffset,
		       nbytes, PTL_ACK_REQ, rpid, GUPCR_PTL_PTE_COLL,
		       PTL_NO_MATCH_BITS, doffset, PTL_NULL_USER_PTR,
		       PTL_NULL_HDR_DATA, op, datatype, gupcr_coll_le_ct,
		       cnt));
}

/**
 * Collectives wait for operation completion
 * This function is used in cases where threads needs to wait
 * for the completion of remote operations.
 *
 * @param [in] cnt Wait count
 */
void
gupcr_coll_ack_wait (size_t cnt)
{
  ptl_ct_event_t ct;
  gupcr_debug (FC_COLL, "wait for %lu (%lu)",
               (long unsigned) cnt,
	       (long unsigned) (gupcr_coll_ack_cnt + cnt));
  gupcr_portals_call (PtlCTWait,
		      (gupcr_coll_md_ct, gupcr_coll_ack_cnt + cnt, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_coll_md_eq);
      gupcr_fatal_error ("received an error on collective MD");
    }
  gupcr_coll_ack_cnt += cnt;
}

/**
 * Collectives wait for signaling events
 * This function is used to wait for other threads to complete
 * operations in the thread's shared space (e.g. children performing
 * atomic ops in the parent's shared space).
 *
 * @param [in] cnt Wait count
 */
void
gupcr_coll_signal_wait (size_t cnt)
{
  ptl_ct_event_t ct;

  gupcr_debug (FC_COLL, "wait for %lu (%lu)",
	       (long unsigned) cnt,
	       (long unsigned) (gupcr_coll_signal_cnt + cnt));
  gupcr_portals_call (PtlCTWait,
		      (gupcr_coll_le_ct, gupcr_coll_signal_cnt + cnt, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_coll_le_eq);
      gupcr_fatal_error ("received an error on collective LE");
    }
  gupcr_coll_signal_cnt += cnt;
}

/**
 * Initialize collectives resources.
 * @ingroup INIT
 *
 * A thread's shared space is mapped via a Portals LE for other
 * threads to write to, and an MD as a source for remote
 * operations.  In this way, the address filed of the shared pointer
 * can be used as an offset into LE/MD.
 */
void
gupcr_coll_init (void)
{
  ptl_md_t md;
  ptl_pt_index_t pte;
  ptl_le_t le;

  gupcr_log (FC_COLL, "coll init called");

  /* Allocate the Portals PTE that is used for collectives.  */
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_coll_le_eq));
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   gupcr_coll_le_eq, GUPCR_PTL_PTE_COLL,
				   &pte));
  if (pte != GUPCR_PTL_PTE_COLL)
    gupcr_fatal_error ("cannot allocate PTE GUPCR_PTL_PTE_COLL.");
  gupcr_debug (FC_COLL, "Collectives PTE allocated: %d", GUPCR_PTL_PTE_COLL);

  /* Allocate the Portals LE that is used for collectives.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_coll_le_ct));
  le.start = gupcr_gmem_base;
  le.length = gupcr_gmem_size;
  le.ct_handle = gupcr_coll_le_ct;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | PTL_LE_EVENT_CT_COMM |
    PTL_LE_EVENT_SUCCESS_DISABLE | PTL_LE_EVENT_LINK_DISABLE;
  gupcr_portals_call (PtlLEAppend, (gupcr_ptl_ni, GUPCR_PTL_PTE_COLL, &le,
				    PTL_PRIORITY_LIST, NULL, &gupcr_coll_le));
  gupcr_debug (FC_COLL, "Collectives LE created at 0x%lx size 0x%lx",
	       (long unsigned) gupcr_gmem_base,
	       (long unsigned) gupcr_gmem_size);

  /* Setup the Portals MD for local source/destination copying.
     We need to map only the shared memory space.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_coll_md_ct));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_coll_md_eq));
  md.start = gupcr_gmem_base;
  md.length = gupcr_gmem_size;
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_CT_REPLY |
    PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_coll_md_eq;
  md.ct_handle = gupcr_coll_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_coll_md));

  /* Reset the number of signals/acks.  */
  gupcr_coll_signal_cnt = 0;
  gupcr_coll_ack_cnt = 0;
}

/**
 * Release collectives resources.
 * @ingroup INIT
 */
void
gupcr_coll_fini (void)
{
  gupcr_log (FC_COLL, "coll fini called");
  /* Release the collectives MD.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_coll_md));
  gupcr_portals_call (PtlCTFree, (gupcr_coll_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_coll_md_eq));
  /* Release the collectives LE and PTE.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_coll_le));
  gupcr_portals_call (PtlCTFree, (gupcr_coll_le_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_coll_le_eq));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_COLL));
}

/** @} */
