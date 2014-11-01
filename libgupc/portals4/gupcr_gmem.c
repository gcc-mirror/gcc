/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
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
 * @file gupcr_gmem.c
 * GUPC Portals4 shared memory interface.
 */

/**
 * @addtogroup GMEM GUPCR Shared Memory Access
 * @{
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_node.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_sync.h"

/** GMEM LE handle */
static ptl_handle_le_t gupcr_gmem_le;

/** Thread's default shared heap size */
#define GUPCR_GMEM_DEFAULT_HEAP_SIZE 256*1024*1024

/** Shared memory base and size */
void *gupcr_gmem_base;
size_t gupcr_gmem_size;

/** GET event tracking */
gupcr_gmem_xfer_info_t gupcr_gmem_gets;
/** PUT event tracking */
gupcr_gmem_xfer_info_t gupcr_gmem_puts;

/** PUT "bounce buffer" type */
typedef char gupcr_gmem_put_bb_t[GUPCR_BOUNCE_BUFFER_SIZE];
/** PUT "bounce buffer" space */
static gupcr_gmem_put_bb_t gupcr_gmem_put_bb;
/** PUT "bounce buffer" memory descriptor handle */
static ptl_handle_md_t gupcr_gmem_put_bb_md;
/** PUT "bounce buffer" used counter */
size_t gupcr_gmem_put_bb_used;

/** Previous operation was a strict put */
int gupcr_pending_strict_put;

/** Heap base offset relative to start of UPC shared region */
size_t gupcr_gmem_heap_base_offset;

/** Size of UPC shared region reserved for the heap */
size_t gupcr_gmem_heap_size;

/** Remote puts flow control */
static const size_t gupcr_gmem_high_mark_puts = GUPCR_MAX_OUTSTANDING_PUTS;
static const size_t gupcr_gmem_low_mark_puts = GUPCR_MAX_OUTSTANDING_PUTS / 2;

/**
 * Allocate memory for this thread's shared space contribution.
 *
 * Calculate needed memory size and let the node allocate
 * shared memory and map other thread's shared memory into
 * the current thread memory space.
 */
static void
gupcr_gmem_alloc_shared (void)
{
  size_t heap_size = GUPCR_ROUND (gupcr_get_shared_heap_size (), C64K);
  size_t data_size = GUPCR_ROUND (GUPCR_SHARED_SECTION_END -
				  GUPCR_SHARED_SECTION_START, C64K);
  gupcr_gmem_heap_base_offset = data_size;
  gupcr_gmem_heap_size = heap_size;
  gupcr_gmem_size = heap_size + data_size;

  /* Allocate this thread's shared space.  */
  gupcr_gmem_base = gupcr_node_local_alloc (gupcr_gmem_size);
}

/**
 * Complete all outstanding remote GET operations.
 *
 * This procedure waits for all outstanding GET operations
 * to complete.  If the wait on the Portals GET counting event returns
 * a failure, a full event queue is checked for failure specifics
 * and the program aborts.
 */
void
gupcr_gmem_sync_gets (void)
{
  /* Sync all outstanding local accesses.  */
  GUPCR_MEM_BARRIER ();
  /* Sync all outstanding remote get accesses.  */
  if (gupcr_gmem_gets.num_pending > 0)
    {
      ptl_size_t num_initiated =
	gupcr_gmem_gets.num_completed + gupcr_gmem_gets.num_pending;
      ptl_ct_event_t ct;
      gupcr_debug (FC_MEM, "outstanding gets: %lu",
		   (long unsigned) gupcr_gmem_gets.num_pending);
      gupcr_portals_call (PtlCTWait,
			  (gupcr_gmem_gets.ct_handle, num_initiated, &ct));
      gupcr_gmem_gets.num_pending = 0;
      gupcr_gmem_gets.num_completed = num_initiated;
      if (ct.failure > 0)
	{
	  gupcr_process_fail_events (gupcr_gmem_gets.eq_handle);
	  gupcr_abort ();
	}
    }
}

/**
 * Complete outstanding remote PUT operations.
 *
 * This procedure waits for all outstanding PUT operations
 * to complete.  If the wait on the Portals PUT counting event returns
 * a failure, a full event queue is checked for failure specifics
 * and the program aborts.
 */
void
gupcr_gmem_sync_puts (void)
{
  /* Sync all outstanding local accesses.  */
  GUPCR_MEM_BARRIER ();
  /* Sync all outstanding remote put accesses.  */
  if (gupcr_gmem_puts.num_pending > 0)
    {
      ptl_size_t num_initiated =
	gupcr_gmem_puts.num_completed + gupcr_gmem_puts.num_pending;
      ptl_ct_event_t ct;
      gupcr_debug (FC_MEM, "outstanding puts: %lu",
		   (long unsigned) gupcr_gmem_puts.num_pending);
      gupcr_portals_call (PtlCTWait,
			  (gupcr_gmem_puts.ct_handle, num_initiated, &ct));
      gupcr_gmem_puts.num_pending = 0;
      gupcr_gmem_puts.num_completed = num_initiated;
      gupcr_pending_strict_put = 0;
      gupcr_gmem_put_bb_used = 0;
      if (ct.failure > 0)
	{
	  gupcr_process_fail_events (gupcr_gmem_puts.eq_handle);
	  gupcr_abort ();
	}
    }
}

/**
 * Complete all outstanding remote operations.
 *
 * Check and wait for completion of all PUT/GET operations.
 */
void
gupcr_gmem_sync (void)
{
  gupcr_gmem_sync_gets ();
  gupcr_gmem_sync_puts ();
}

/**
 * Read data from remote shared memory.
 *
 * A GET request is broken into multiple PtlGet() requests
 * if the number of requested bytes is greater then
 * the configuration limited maximum message size.
 *
 * @param [in] dest Local memory to receive remote data
 * @param [in] thread Remote thread to request data from
 * @param [in] offset Remote address
 * @param [in] n Number of bytes to transfer
 */
void
gupcr_gmem_get (void *dest, int thread, size_t offset, size_t n)
{
  ptl_process_t rpid;
  char *dest_addr = (char *) (dest - USER_PROG_MEM_START);
  size_t rem_offset = offset;
  size_t n_rem = n;

  gupcr_debug (FC_MEM, "%d:0x%lx 0x%lx",
	       thread, (long unsigned) offset, (long unsigned) dest);
  rpid.rank = thread;
  while (n_rem > 0)
    {
      size_t n_xfer;
      n_xfer = GUPCR_MIN (n_rem, (size_t) GUPCR_PORTALS_MAX_MSG_SIZE);
      ++gupcr_gmem_gets.num_pending;
      gupcr_portals_call (PtlGet, (gupcr_gmem_gets.md,
				   (ptl_size_t) dest_addr, n_xfer, rpid,
				   GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				   rem_offset, PTL_NULL_USER_PTR));
      n_rem -= n_xfer;
      dest_addr += n_xfer;
      rem_offset += n_xfer;
    }
}

/**
 * Write data to remote shared memory.
 *
 * For data requests smaller then maximum safe size, the data is first
 * copied into a bounce buffer.  In this way, the put operation
 * can be non-blocking and there are no restrictions placed upon
 * the caller's use of the source data buffer.
 * Otherwise,  a synchronous operation is performed
 * and this function returns to the caller after the operation completes.
 *
 * @param [in] thread Destination thread
 * @param [in] offset Destination offset
 * @param [in] src Local source pointer to data
 * @param [in] n Number of bytes to transfer
 */
void
gupcr_gmem_put (int thread, size_t offset, const void *src, size_t n)
{
  int must_sync = (n > GUPCR_GMEM_MAX_SAFE_PUT_SIZE);
  char *src_addr = (char *) src;
  size_t n_rem = n;
  ptl_process_t rpid;
  gupcr_debug (FC_MEM, "0x%lx %d:0x%lx",
                       (long unsigned) src, thread, (long unsigned) offset);
  rpid.rank = thread;
  /* Large puts must be synchronous, to ensure that it is
     safe to re-use the source buffer upon return.  */
  while (n_rem > 0)
    {
      size_t n_xfer;
      ptl_handle_md_t md_handle;
      ptl_size_t local_offset;
      n_xfer = GUPCR_MIN (n_rem, (size_t) GUPCR_PORTALS_MAX_MSG_SIZE);
      if (must_sync)
	{
	  local_offset = src_addr - (char *) USER_PROG_MEM_START;
	  md_handle = gupcr_gmem_puts.md;
	}
      else if (n_rem <= GUPCR_PORTALS_MAX_VOLATILE_SIZE)
	{
	  local_offset = src_addr - (char *) USER_PROG_MEM_START;
	  md_handle = gupcr_gmem_puts.md_volatile;
	}
      else
	{
	  char *bounce_buf;
	  /* If this transfer will overflow the bounce buffer,
	     then first wait for all outstanding puts to complete.  */
	  if ((gupcr_gmem_put_bb_used + n_xfer) > GUPCR_BOUNCE_BUFFER_SIZE)
	    gupcr_gmem_sync_puts ();
	  bounce_buf = &gupcr_gmem_put_bb[gupcr_gmem_put_bb_used];
	  memcpy (bounce_buf, src_addr, n_xfer);
	  local_offset = bounce_buf - gupcr_gmem_put_bb;
	  gupcr_gmem_put_bb_used += n_xfer;
	  md_handle = gupcr_gmem_put_bb_md;
	}
      ++gupcr_gmem_puts.num_pending;
      gupcr_portals_call (PtlPut, (md_handle, local_offset, n_xfer,
				   PTL_ACK_REQ, rpid,
				   GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				   offset, PTL_NULL_USER_PTR,
				   PTL_NULL_HDR_DATA));
      n_rem -= n_xfer;
      src_addr += n_xfer;

      if (gupcr_gmem_puts.num_pending == gupcr_gmem_high_mark_puts)
   	{
	  ptl_ct_event_t ct;
	  size_t complete_cnt;
	  size_t wait_cnt = gupcr_gmem_puts.num_completed
			    + gupcr_gmem_puts.num_pending
			    - gupcr_gmem_low_mark_puts;
	  gupcr_portals_call (PtlCTWait,
			      (gupcr_gmem_puts.ct_handle, wait_cnt, &ct));
	  if (ct.failure > 0)
	    {
	      gupcr_process_fail_events (gupcr_gmem_puts.eq_handle);
	      gupcr_abort ();
	    }
	  complete_cnt = ct.success - gupcr_gmem_puts.num_completed;
	  gupcr_gmem_puts.num_pending -= complete_cnt;
	  gupcr_gmem_puts.num_completed = ct.success;
	}
    }
  if (must_sync)
    gupcr_gmem_sync_puts ();
}

/**
 * Copy remote shared memory from the source thread
 * to the destination thread.
 *
 * Bulk copy from one thread to another.
 * The put bounce buffer is used as an intermediate buffer.
 * Caller assumes responsibility for checking the validity
 * of the remote thread id's and/or shared memory offsets.
 *
 * @param [in] dthread Destination thread
 * @param [in] doffset Destination offset
 * @param [in] sthread Source thread
 * @param [in] soffset Source offset
 * @param [in] n Number of bytes to transfer
 */
void
gupcr_gmem_copy (int dthread, size_t doffset,
		 int sthread, size_t soffset, size_t n)
{
  size_t n_rem = n;
  ptl_size_t dest_addr = doffset;
  ptl_size_t src_addr = soffset;
  ptl_process_t dpid;
  gupcr_debug (FC_MEM, "%d:0x%lx %d:0x%lx %lu",
	       sthread, (long unsigned) soffset,
	       dthread, (long unsigned) doffset,
	       (long unsigned) n);
  dpid.rank = dthread;
  while (n_rem > 0)
    {
      size_t n_xfer;
      char *bounce_buf;
      ptl_size_t local_offset;
      /* Use the entire put "bounce buffer" if the transfer
         count is sufficiently large.  */
      n_xfer = GUPCR_MIN (n_rem, GUPCR_BOUNCE_BUFFER_SIZE);
      if ((gupcr_gmem_put_bb_used + n_xfer) > GUPCR_BOUNCE_BUFFER_SIZE)
	gupcr_gmem_sync_puts ();
      bounce_buf = &gupcr_gmem_put_bb[gupcr_gmem_put_bb_used];
      gupcr_gmem_put_bb_used += n_xfer;
      /* Read the source data into the bounce buffer.  */
      gupcr_gmem_get (bounce_buf, sthread, src_addr, n_xfer);
      gupcr_gmem_sync_gets ();
      local_offset = bounce_buf - gupcr_gmem_put_bb;
      ++gupcr_gmem_puts.num_pending;
      gupcr_portals_call (PtlPut, (gupcr_gmem_put_bb_md, local_offset, n_xfer,
				   PTL_ACK_REQ, dpid,
				   GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				   dest_addr, PTL_NULL_USER_PTR,
				   PTL_NULL_HDR_DATA));
      n_rem -= n_xfer;
      src_addr += n_xfer;
      dest_addr += n_xfer;
    }
}

/**
 * Write the same byte value into the bytes of the
 * destination thread's memory at the specified offset.
 *
 * The put bounce buffer is used as an intermediate buffer.
 * The last write of a chunk of data is non-blocking.
 * Caller assumes responsibility for checking the validity
 * of the remote thread id's and/or shared memory offsets.
 *
 * @param [in] thread Destination thread
 * @param [in] offset Destination offset
 * @param [in] c Set value
 * @param [in] n Number of bytes to transfer
 */
void
gupcr_gmem_set (int thread, size_t offset, int c, size_t n)
{
  size_t n_rem = n;
  int already_filled = 0;
  ptl_size_t dest_addr = offset;
  ptl_process_t rpid;
  gupcr_debug (FC_MEM, "0x%x %d:0x%lx %lu", c, thread,
                       (long unsigned) offset, (long unsigned) n);
  rpid.rank = thread;
  while (n_rem > 0)
    {
      size_t n_xfer;
      char *bounce_buf;
      ptl_size_t local_offset;
      /* Use the entire put "bounce buffer" if the transfer
         count is sufficiently large.  */
      n_xfer = GUPCR_MIN (n_rem, (size_t) GUPCR_BOUNCE_BUFFER_SIZE);
      if ((gupcr_gmem_put_bb_used + n_xfer) > GUPCR_BOUNCE_BUFFER_SIZE)
	gupcr_gmem_sync_puts ();
      bounce_buf = &gupcr_gmem_put_bb[gupcr_gmem_put_bb_used];
      gupcr_gmem_put_bb_used += n_xfer;
      /* Fill the bounce buffer, if we haven't already.  */
      if (!already_filled)
	{
	  memset (bounce_buf, c, n_xfer);
	  already_filled = (bounce_buf == gupcr_gmem_put_bb
			    && n_xfer == GUPCR_BOUNCE_BUFFER_SIZE);
	}
      local_offset = bounce_buf - gupcr_gmem_put_bb;
      ++gupcr_gmem_puts.num_pending;
      gupcr_portals_call (PtlPut, (gupcr_gmem_put_bb_md, local_offset, n_xfer,
				   PTL_ACK_REQ, rpid,
				   GUPCR_PTL_PTE_GMEM, PTL_NO_MATCH_BITS,
				   dest_addr, PTL_NULL_USER_PTR,
				   PTL_NULL_HDR_DATA));
      n_rem -= n_xfer;
      dest_addr += n_xfer;
    }
}

/**
 * Initialize gmem resources.
 * @ingroup INIT
 */
void
gupcr_gmem_init (void)
{
  ptl_md_t md, md_volatile;
  ptl_le_t le;
  ptl_pt_index_t pte;
  gupcr_log (FC_MEM, "gmem init called");
  /* Allocate memory for this thread's contribution to shared memory.  */
  gupcr_gmem_alloc_shared ();
  gupcr_portals_call (PtlPTAlloc,
		      (gupcr_ptl_ni, 0,
		       PTL_EQ_NONE, GUPCR_PTL_PTE_GMEM, &pte));
  if (pte != GUPCR_PTL_PTE_GMEM)
    gupcr_fatal_error ("cannot allocate PTE GUPCR_PTL_PTE_GMEM");
  gupcr_log (FC_MEM, "Gmem PTE allocated: %d", GUPCR_PTL_PTE_GMEM);
  /* Setup Gmem LE.  */
  le.start = gupcr_gmem_base;
  le.length = gupcr_gmem_size;
  le.ct_handle = PTL_CT_NONE;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET;
  gupcr_portals_call (PtlLEAppend,
		      (gupcr_ptl_ni,
		       GUPCR_PTL_PTE_GMEM, &le,
		       PTL_PRIORITY_LIST, NULL, &gupcr_gmem_le));
  gupcr_debug (FC_MEM, "Gmem LE created at 0x%lx with size 0x%lx)",
	      (long unsigned) gupcr_gmem_base,
	      (long unsigned) gupcr_gmem_size);
  /* Initialize GMEM get lists */
  gupcr_gmem_gets.num_pending = 0;
  gupcr_gmem_gets.num_completed = 0;
  gupcr_gmem_gets.md_options =
    PTL_MD_EVENT_CT_REPLY | PTL_MD_EVENT_SUCCESS_DISABLE;
  /* Allocate at least THREADS number of EQ entries.  */
  gupcr_portals_call (PtlEQAlloc,
		      (gupcr_ptl_ni, THREADS, &gupcr_gmem_gets.eq_handle));
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_gmem_gets.ct_handle));
  /* Map user's address space for GET operations.  */
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options = gupcr_gmem_gets.md_options;
  md.eq_handle = gupcr_gmem_gets.eq_handle;
  md.ct_handle = gupcr_gmem_gets.ct_handle;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_gmem_gets.md));
  /* Initialize GMEM put lists.  */
  gupcr_gmem_puts.num_pending = 0;
  gupcr_gmem_puts.num_completed = 0;
  gupcr_gmem_puts.md_options =
    PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  /* Allocate at least THREADS number of EQ entries.  */
  gupcr_portals_call (PtlEQAlloc,
		      (gupcr_ptl_ni, THREADS, &gupcr_gmem_puts.eq_handle));
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_gmem_puts.ct_handle));
  /* Map user's address space for PUT operations.  */
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options = gupcr_gmem_puts.md_options;
  md.eq_handle = gupcr_gmem_puts.eq_handle;
  md.ct_handle = gupcr_gmem_puts.ct_handle;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_gmem_puts.md));
  /* And map the same but with a volatile option.  */
  md_volatile = md;
  md_volatile.options |= PTL_MD_VOLATILE;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md_volatile,
				  &gupcr_gmem_puts.md_volatile));
  /* Initialize GMEM put bounce buffer.  */
  md.length = GUPCR_BOUNCE_BUFFER_SIZE;
  md.start = gupcr_gmem_put_bb;
  md.options = gupcr_gmem_puts.md_options;
  md.eq_handle = gupcr_gmem_puts.eq_handle;
  md.ct_handle = gupcr_gmem_puts.ct_handle;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_gmem_put_bb_md));
}

/**
 * Release gmem resources.
 * @ingroup INIT
 */
void
gupcr_gmem_fini (void)
{
  gupcr_log (FC_MEM, "gmem fini called");
  /* Release GET MD.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_gmem_gets.md));
  gupcr_portals_call (PtlCTFree, (gupcr_gmem_gets.ct_handle));
  gupcr_portals_call (PtlEQFree, (gupcr_gmem_gets.eq_handle));
  /* Release PUT MDs.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_gmem_puts.md));
  gupcr_portals_call (PtlMDRelease, (gupcr_gmem_put_bb_md));
  gupcr_portals_call (PtlCTFree, (gupcr_gmem_puts.ct_handle));
  gupcr_portals_call (PtlEQFree, (gupcr_gmem_puts.eq_handle));
  /* Release LEs and PTEs.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_gmem_le));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_GMEM));
}

/** @} */
