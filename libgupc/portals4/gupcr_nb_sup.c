/* Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include "malloc.h"
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_lib.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_nb_sup.h"

/**
 * @file gupcr_nb_sup.c
 * GUPC Portals4 non-blocking transfers support routines.
 *
 * @addtogroup NON-BLOCKING GUPCR Non-Blocking Transfer Support Functions
 * @{
 */

/** Non-blocking shared access LE handle */
static ptl_handle_le_t gupcr_nb_le;
/** Explicit non-blocking MD handle */
static ptl_handle_md_t gupcr_nb_md;
/** Explicit non-blocking MD event queue handle */
static ptl_handle_eq_t gupcr_nb_md_eq;
/** Start of the explicit non-blocking MD */
static char *gupcr_nb_md_start;

/** Implicit non-blocking MD handle */
static ptl_handle_md_t gupcr_nbi_md;
/** Implicit non-blocking MD EQ handle */
static ptl_handle_eq_t gupcr_nbi_md_eq;
/** Implicit non-blocking MD CT handle */
static ptl_handle_ct_t gupcr_nbi_md_ct;
/** Implicit non-blocking number of received ACKs on local md */
static ptl_size_t gupcr_nbi_md_count;
/** Start of the implicit non-blocking MD */
static char *gupcr_nbi_md_start;

/* All non-blocking transfers with explicit handle
   are managed through the 'gupcr_nbcb' structure
   (control block).  Free control blocks are on
   the free list, while those with active transfers
   are on the 'active' list.  A single linked list
   is used to link CBs on the free or active list.

   Non-blocking transfer handle is an unsigned long
   number that we increment every time a new transfer is
   requested.  */

/** Non-blocking transfers control block */
struct gupcr_nbcb
{
  struct gupcr_nbcb *next; /** forward link on the free or used list */
  unsigned long id; /** UPC handle for non-blocking transfer */
  int status; /** non-blocking transfer status */
};
typedef struct gupcr_nbcb gupcr_nbcb_t;
typedef struct gupcr_nbcb *gupcr_nbcb_p;

/** nb transfer status value */
#define	NB_STATUS_COMPLETED 1
#define	NB_STATUS_NOT_COMPLETED 0

/** NB handle values */
unsigned long gupcr_nb_handle_next;

/** NB cb free list */
gupcr_nbcb_p gupcr_nbcb_cb_free = NULL;
/** List of NB active transfers */
gupcr_nbcb_p gupcr_nbcb_active = NULL;

/** Number of outstanding transfers with explicit handle */
int gupcr_nb_outstanding;
void gupcr_nb_check_outstanding (void);

/**
 * Allocate free NB control block
 */
static gupcr_nbcb_p
gupcr_nbcb_alloc (void)
{
  gupcr_nbcb_p cb;
  if (gupcr_nbcb_cb_free)
    {
      cb = gupcr_nbcb_cb_free;
      gupcr_nbcb_cb_free = cb->next;
    }
  else
    {
      /* Allocate memory for the new block.  */
      cb = calloc (sizeof (struct gupcr_nbcb), 1);
      if (cb == NULL)
	gupcr_fatal_error ("cannot allocate local memory");
    }
  return cb;
}

/**
 * Place NB control block on the free list
 */
static void
gupcr_nbcb_free (gupcr_nbcb_p cb)
{
  cb->next = gupcr_nbcb_cb_free;
  gupcr_nbcb_cb_free = cb;
}

/**
 * Place NB control block on the active list
 */
static void
gupcr_nbcb_active_insert (gupcr_nbcb_p cb)
{
  cb->next = gupcr_nbcb_active;
  gupcr_nbcb_active = cb;
}

/**
 * Remove NB control block from the active list
 */
static void
gupcr_nbcb_active_remove (gupcr_nbcb_p cb)
{
  gupcr_nbcb_p acb = gupcr_nbcb_active;
  gupcr_nbcb_p prev_acb = acb;
  while (acb)
    {
      if (acb == cb)
	{
	  if (acb == gupcr_nbcb_active)
	    gupcr_nbcb_active = acb->next;
	  else
	    prev_acb->next = acb->next;
	  return;
	}
      prev_acb = acb;
      acb = acb->next;
    }
}

/**
 * Find NB control block on the active list
 */
static gupcr_nbcb_p
gupcr_nbcb_find (unsigned long id)
{
  gupcr_nbcb_p cb = gupcr_nbcb_active;
  while (cb)
    {
      if (cb->id == id)
	return cb;
      cb = cb->next;
    }
  return NULL;
}

/**
 * Non-blocking GET operation
 *
 * @param[in] sthread Source thread
 * @param[in] soffset Source offset
 * @param[in] dst_ptr Destination local pointer
 * @param[in] size Number of bytes to transfer
 * @param[in] handle Transfer handle (NULL for implicit)
 */
void
gupcr_nb_get (size_t sthread, size_t soffset, char *dst_ptr,
	      size_t size, unsigned long *handle)
{
  ptl_process_t rpid;
  size_t n_rem = size;
  ptl_size_t local_offset = dst_ptr - gupcr_nbi_md_start;

  if (handle)
    {
      gupcr_nbcb_p cb = gupcr_nbcb_alloc ();
      cb->id = gupcr_nb_handle_next++;
      cb->status = NB_STATUS_NOT_COMPLETED;
      gupcr_nbcb_active_insert (cb);
      *handle = cb->id;
      gupcr_nb_check_outstanding ();
    }
  gupcr_debug (FC_NB, "%s %lu:0x%lx(%ld) -> 0x%lx (%lu)",
	       handle ? "NB" : "NBI", sthread, soffset,
	       size, (long unsigned int) dst_ptr, handle ? *handle : 0);

  /* Large transfers must be done in chunks.  Only the last chunk
     behaves as a non-blocking transfer.  */
  while (n_rem > 0)
    {
      size_t n_xfer;
      n_xfer = GUPCR_MIN (n_rem, GUPCR_PORTALS_MAX_MSG_SIZE);
      rpid.rank = sthread;
      gupcr_portals_call (PtlGet, (handle ? gupcr_nb_md : gupcr_nbi_md,
				   local_offset,
				   n_xfer, rpid, GUPCR_PTL_PTE_NB,
				   PTL_NO_MATCH_BITS, soffset,
				   handle ? (void *) *handle : NULL));
      if (handle)
        gupcr_nb_outstanding += 1;
      else
        gupcr_nbi_md_count += 1;
      n_rem -= n_xfer;
      local_offset += n_xfer;
      if (n_rem)
	{
	  /* Unfortunately, there are more data to transfer, we have to
	     wait for all non-blocking transfers to complete.  */
	  if (handle)
	    gupcr_sync (*handle);
	  else
	    gupcr_synci ();
	}
    }
}

/**
 * Non-blocking transfer PUT operation
 *
 * @param[in] dthread Destination thread
 * @param[in] doffset Destination offset
 * @param[in] src_ptr Source local pointer
 * @param[in] size Number of bytes to transfer
 * @param[in] handle Transfer handle (NULL for implicit)
 */
void
gupcr_nb_put (size_t dthread, size_t doffset, const void *src_ptr,
	      size_t size, unsigned long *handle)
{
  ptl_process_t rpid;
  size_t n_rem = size;
  ptl_size_t local_offset = (char *) src_ptr - gupcr_nbi_md_start;

  if (handle)
    {
      gupcr_nbcb_p cb = gupcr_nbcb_alloc ();
      cb->id = gupcr_nb_handle_next++;
      cb->status = NB_STATUS_NOT_COMPLETED;
      gupcr_nbcb_active_insert (cb);
      *handle = cb->id;
      gupcr_nb_check_outstanding ();
    }

  gupcr_debug (FC_NB, "%s 0x%lx(%ld) -> %lu:0x%lx (%lu)",
	       handle ? "NB" : "NBI", (long unsigned int) src_ptr, size,
	       dthread, doffset, handle ? *handle : 0);

  /* Large transfers must be done in chunks.  Only the last chunk
     behaves as a non-blocking transfer.  */
  while (n_rem > 0)
    {
      size_t n_xfer;
      n_xfer = GUPCR_MIN (n_rem, GUPCR_PORTALS_MAX_MSG_SIZE);
      rpid.rank = dthread;
      gupcr_portals_call (PtlPut, (handle ? gupcr_nb_md : gupcr_nbi_md,
				   local_offset, n_xfer, PTL_ACK_REQ, rpid,
				   GUPCR_PTL_PTE_NB, PTL_NO_MATCH_BITS,
				   doffset, handle ? (void *) *handle : NULL,
				   PTL_NULL_HDR_DATA));
      if (handle)
        gupcr_nb_outstanding += 1;
      else
        gupcr_nbi_md_count += 1;
      n_rem -= n_xfer;
      local_offset += n_xfer;
      if (n_rem)
	{
	  /* Unfortunately, there are more data to transfer, we have to
	     wait for all non-blocking transfers to complete.  */
	  if (handle)
	    gupcr_sync (*handle);
	  else
	    gupcr_synci ();
	}
    }
}

/**
 * Check for the max number of outstanding non-blocking
 * transfers with explicit handle
 *
 * We cannot allow for number of outstanding transfers
 * to go over the event queue size.  Otherwise, some ACK/REPLY
 * can be dropped.
 */
void
gupcr_nb_check_outstanding (void)
{
  if (gupcr_nb_outstanding == GUPCR_NB_MAX_OUTSTANDING)
    {
      /* We have to wait for at least one to complete.  */
      ptl_event_t event;
      gupcr_portals_call (PtlEQWait, (gupcr_nb_md_eq, &event));

      /* Process only ACKs and REPLYs,  */
      if (event.type == PTL_EVENT_ACK || event.type == PTL_EVENT_REPLY)
	{
	  gupcr_nbcb_p cb;
	  unsigned long id = (unsigned long) event.user_ptr;
	  gupcr_debug (FC_NB, "received event for handle %lu", id);
	  cb = gupcr_nbcb_find (id);
	  if (!cb || cb->status == NB_STATUS_COMPLETED)
	    {
	      gupcr_fatal_error
		("received event for unexistent or already completed"
		 " NB handle");
	    }
	  cb->status = NB_STATUS_COMPLETED;
	  gupcr_nb_outstanding--;
	}
      else
	{
	  gupcr_fatal_error ("received event of invalid type: %s",
			      gupcr_streqtype (event.type));
	}
    }
}

/**
 * Check for non-blocking transfer complete
 *
 * @param[in] handle Transfer handle
 * @retval "1" if transfer completed
 */
int
gupcr_nb_completed (unsigned long handle)
{
  ptl_event_t event;
  int done = 0;
  gupcr_nbcb_p cb;

  /* Handle Portals completion events.  */
  while (!done)
    {
      int pstatus;
      gupcr_portals_call_with_status (PtlEQGet, pstatus,
				      (gupcr_nb_md_eq, &event));
      if (pstatus == PTL_OK)
	{
	  /* There is something to process.  */
	  if (event.type == PTL_EVENT_ACK || event.type == PTL_EVENT_REPLY)
	    {
	      unsigned long id = (unsigned long) event.user_ptr;
	      gupcr_debug (FC_NB, "received event for handle %lu", id);
	      cb = gupcr_nbcb_find (id);
	      if (!cb)
		gupcr_fatal_error ("received event for invalid NB handle");
	      cb->status = NB_STATUS_COMPLETED;
	      gupcr_nb_outstanding--;
	    }
	  else
	    {
	      gupcr_fatal_error ("received event of invalid type: %s",
				 gupcr_streqtype (event.type));
	    }
	}
      else
	done = 1;
    }

  /* Check if transfer is completed.  */
  cb = gupcr_nbcb_find (handle);
  if (cb && cb->status == NB_STATUS_COMPLETED)
    {
      gupcr_nbcb_active_remove (cb);
      gupcr_nbcb_free (cb);
      return 1;
    }

  return 0;
}

/**
 * Complete non-blocking transfers with explicit handle
 *
 * Wait for outstanding request to complete.
 *
 * @param[in] handle Transfer handle
 */
void
gupcr_sync (unsigned long handle)
{
  gupcr_nbcb_p cb;

  gupcr_debug (FC_NB, "waiting for handle %lu", handle);
  /* Check if transfer already completed.  */
  cb = gupcr_nbcb_find (handle);
  if (!cb)
    {
      /* Handle does not exist.  Assume it is a duplicate
         sync request.  */
      return;
    }
  if (cb->status == NB_STATUS_COMPLETED)
    {
      /* Already completed.  */
      gupcr_nbcb_active_remove (cb);
      gupcr_nbcb_free (cb);
    }
  else
    {
      int done = 0;
      /* Must wait for portals to complete the transfer.  */
      while (!done)
	{
	  ptl_event_t event;
	  int pstatus;
	  gupcr_portals_call_with_status (PtlEQGet, pstatus,
					  (gupcr_nb_md_eq, &event));
	  if (pstatus == PTL_OK)
	    {
	      /* Process only ACKs and REPLYs,  */
	      gupcr_debug (FC_NB, "received event of type %s",
			   gupcr_streqtype (event.type));
	      if (event.type == PTL_EVENT_ACK
		  || event.type == PTL_EVENT_REPLY)
		{
		  unsigned long id = (unsigned long) event.user_ptr;
		  gupcr_debug (FC_NB, "received event for handle %lu", id);
		  cb = gupcr_nbcb_find (id);
		  if (!cb || cb->status == NB_STATUS_COMPLETED)
		    {
		      gupcr_fatal_error
			("received event for unexistent or already completed"
			 " NB handle");
		    }
		  cb->status = NB_STATUS_COMPLETED;
		  gupcr_nb_outstanding--;
		  if (id == handle)
		    {
		      gupcr_nbcb_active_remove (cb);
		      gupcr_nbcb_free (cb);
		      done = 1;
		    }
		}
	      else
		{
		  gupcr_fatal_error ("received event of invalid type: %s",
				     gupcr_streqtype (event.type));
		}
	    }
	}
    }
}

/**
 * Check for any outstanding implicit handle non-blocking transfer
 *
 * @retval Number of outstanding transfers
 */
int
gupcr_nbi_outstanding (void)
{
  ptl_ct_event_t ct;

  /* Check the number of completed transfers.  */
  gupcr_portals_call (PtlCTGet, (gupcr_nbi_md_ct, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_nbi_md_eq);
      gupcr_fatal_error ("received an error on NBI MD");
    }
  return (int) (gupcr_nbi_md_count - ct.success);
}

/**
 * Complete non-blocking transfers with implicit handle
 *
 * Wait for all outstanding requests to complete.
 */
void
gupcr_synci (void)
{
  ptl_ct_event_t ct;
  gupcr_portals_call (PtlCTWait, (gupcr_nbi_md_ct, gupcr_nbi_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_nbi_md_eq);
      gupcr_fatal_error ("received an error on NBI MD");
    }
}

/**
 * Initialize non-blocking transfer resources
 * @ingroup INIT
 */
void
gupcr_nb_init (void)
{
  ptl_md_t md;
  ptl_pt_index_t pte;
  ptl_le_t le;

  gupcr_log (FC_NB, "non-blocking transfer init called");

  /* Non-blocking transfers use their own PTE.  */
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   PTL_EQ_NONE, GUPCR_PTL_PTE_NB, &pte));
  if (pte != GUPCR_PTL_PTE_NB)
    gupcr_fatal_error ("cannot allocate PTE GUPCR_PTL_PTE_NB.");
  gupcr_debug (FC_NB, "Non-blocking PTE allocated: %d", GUPCR_PTL_PTE_NB);
  /* Allocate LE for non-blocking transfers.  */
  le.start = gupcr_gmem_base;
  le.length = gupcr_gmem_size;
  le.ct_handle = PTL_CT_NONE;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET;
  gupcr_portals_call (PtlLEAppend,
		      (gupcr_ptl_ni, GUPCR_PTL_PTE_NB, &le,
		       PTL_PRIORITY_LIST, NULL, &gupcr_nb_le));
  gupcr_debug (FC_NB,
	       "Non-blocking LE created at 0x%lx with size 0x%lx)",
	       (long unsigned) gupcr_gmem_base,
	       (long unsigned) gupcr_gmem_size);

  /* Setup the Portals MD for local source/destination copying.
     We need to map the whole user's space (same as gmem).  */
  /* Non-blocking transfers with explicit handles must use full events
     there is no need for counting events.  */
  gupcr_portals_call (PtlEQAlloc,
		      (gupcr_ptl_ni, GUPCR_NB_MAX_OUTSTANDING,
		       &gupcr_nb_md_eq));
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options = PTL_MD_EVENT_SEND_DISABLE;
  md.eq_handle = gupcr_nb_md_eq;
  md.ct_handle = PTL_CT_NONE;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_nb_md));
  gupcr_nb_md_start = md.start;
  /* Non-blocking transfers with implicit handles use counting events.  */
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_nbi_md_eq));
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_nbi_md_ct));
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_CT_REPLY |
    PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_nbi_md_eq;
  md.ct_handle = gupcr_nbi_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_nbi_md));
  gupcr_nbi_md_start = md.start;

  /* Reset number of acknowledgments.  */
  gupcr_nbi_md_count = 0;

  /* Initialize NB handle values.  */
  gupcr_nb_handle_next = 1;
  /* Initialize number of outstanding transfers.  */
  gupcr_nb_outstanding = 0;
}

/**
 * Release non-blocking transfer resources
 * @ingroup INIT
 */
void
gupcr_nb_fini (void)
{
  gupcr_log (FC_NB, "non-blocking transfer fini called");
  /* Release explicit handle NB MD and its resources.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_nb_md));
  gupcr_portals_call (PtlEQFree, (gupcr_nb_md_eq));
  /* Release implicit handle NB MD and its resources.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_nbi_md));
  gupcr_portals_call (PtlEQFree, (gupcr_nbi_md_eq));
  gupcr_portals_call (PtlCTFree, (gupcr_nbi_md_ct));
  /* Release LE and PTE.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_nb_le));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_NB));
}

/** @} */
