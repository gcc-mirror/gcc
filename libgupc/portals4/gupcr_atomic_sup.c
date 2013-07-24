/* Copyright (C) 2013 Free Software Foundation, Inc.
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
#include "gupcr_atomic_sup.h"

/**
 * @file gupcr_atomic_sup.c
 * GUPC Portals4 atomic support routines.
 *
 * @addtogroup ATOMIC GUPCR Atomics Support Functions
 * @{
 */

/** Atomic local access MD handle */
static ptl_handle_md_t gupcr_atomic_md;
/** Atomic local access MD counting events handle */
static ptl_handle_ct_t gupcr_atomic_md_ct;
/** Atomic local access MD event queue handle */
static ptl_handle_eq_t gupcr_atomic_md_eq;
/** Atomic number of received ACKs on local md */
static ptl_size_t gupcr_atomic_md_count;

/** Atomic operations use remote gmem PTE */
#define GUPCR_PTL_PTE_ATOMIC GUPCR_PTL_PTE_GMEM

/**
 * Atomic GET operation.
 *
 * A simple Portals4 get operation is sufficient for data
 * types supported by UPC.
 *
 * @param[in] thread Destination thread
 * @param[in] doffset Destination offset
 * @param[in] fetch_ptr Fetch value pointer
 * @param[in] type Atomic data type
 */
void
gupcr_atomic_get (size_t dthread, size_t doffset, void *fetch_ptr,
		  ptl_datatype_t type)
{
  ptl_ct_event_t ct;
  ptl_process_t rpid;
  char tmpbuf[128] __attribute__ ((unused));
  size_t size;

  gupcr_debug (FC_ATOMIC, "%lu:0x%lx", dthread, doffset);
  if (fetch_ptr == NULL)
    gupcr_error ("UPC_GET fetch pointer is NULL");

  size = gupcr_get_atomic_size (type);
  rpid.rank = dthread;
  gupcr_portals_call (PtlGet, (gupcr_atomic_md, (ptl_size_t) fetch_ptr,
			       size, rpid, GUPCR_PTL_PTE_ATOMIC,
			       PTL_NO_MATCH_BITS, doffset,
			       PTL_NULL_USER_PTR));
  gupcr_atomic_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_atomic_md_ct, gupcr_atomic_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_atomic_md_eq);
      gupcr_fatal_error ("received an error on atomic MD");
    }
  gupcr_debug (FC_ATOMIC, "ov(%s)",
	       gupcr_get_buf_as_hex (tmpbuf, fetch_ptr, size));
}

/**
 * Portals4 atomic set operation.
 *
 * Execute Portals4 PtlSwap with PTL_SWAP operation.
 *
 * @param[in] thread Destination thread
 * @param[in] doffset Destination offset
 * @param[in] fetch_ptr Fetch value pointer (optional)
 * @param[in] value New value of atomic variable
 * @param[in] type Atomic data type
 */
void
gupcr_atomic_set (size_t dthread, size_t doffset, void *fetch_ptr,
		  const void *value, ptl_datatype_t type)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  char tmpbuf[128] __attribute__ ((unused));
  char atomic_tmp_buf[GUPC_MAX_ATOMIC_SIZE];
  size_t size = gupcr_get_atomic_size (type);
  gupcr_debug (FC_ATOMIC, "%lu:0x%lx v(%s)", dthread, doffset,
	       gupcr_get_buf_as_hex (tmpbuf, value, size));
  rpid.rank = dthread;
  gupcr_portals_call (PtlSwap, (gupcr_atomic_md,
				(ptl_size_t) atomic_tmp_buf,
				gupcr_atomic_md, (ptl_size_t) value,
				size, rpid, GUPCR_PTL_PTE_ATOMIC,
				PTL_NO_MATCH_BITS, doffset, PTL_NULL_USER_PTR,
				PTL_NULL_HDR_DATA, NULL, PTL_SWAP, type));
  gupcr_atomic_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_atomic_md_ct, gupcr_atomic_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_atomic_md_eq);
      gupcr_fatal_error ("received an error on atomic MD");
    }
  if (fetch_ptr)
    {
      gupcr_debug (FC_ATOMIC, "ov(%s)",
		   gupcr_get_buf_as_hex (tmpbuf, atomic_tmp_buf, size));
      memcpy (fetch_ptr, atomic_tmp_buf, size);
    }
}

/**
 * Portals4 atomic CSWAP operation.
 *
 * Execute Portals4 PtlSwap with PTL_CSWAP operation.
 *
 * @param[in] thread Destination thread
 * @param[in] doffset Destination offset
 * @param[in] fetch_ptr Fetch value pointer (optional)
 * @param[in] expected Expected value of atomic variable
 * @param[in] value New value of atomic variable
 * @param[in] type Atomic data type
 */
void
gupcr_atomic_cswap (size_t dthread, size_t doffset, void *fetch_ptr,
		    const void *expected, const void *value,
		    ptl_datatype_t type)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  char tmpbuf[128] __attribute__ ((unused));
  char atomic_tmp_buf[GUPC_MAX_ATOMIC_SIZE];
  size_t size = gupcr_get_atomic_size (type);
  gupcr_debug (FC_ATOMIC, "%lu:0x%lx v(%s) e(%s)", dthread, doffset,
	       gupcr_get_buf_as_hex (tmpbuf, value, size),
	       gupcr_get_buf_as_hex (tmpbuf, expected, size));
  rpid.rank = dthread;
  gupcr_portals_call (PtlSwap, (gupcr_atomic_md,
				(ptl_size_t) atomic_tmp_buf,
				gupcr_atomic_md, (ptl_size_t) value,
				size, rpid,
				GUPCR_PTL_PTE_ATOMIC, PTL_NO_MATCH_BITS,
				doffset, PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA,
				expected, PTL_CSWAP, type));
  gupcr_atomic_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_atomic_md_ct, gupcr_atomic_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_atomic_md_eq);
      gupcr_fatal_error ("received an error on atomic MD");
    }
  if (fetch_ptr)
    {
      gupcr_debug (FC_ATOMIC, "ov(%s)",
		   gupcr_get_buf_as_hex (tmpbuf, atomic_tmp_buf, size));
      memcpy (fetch_ptr, atomic_tmp_buf, size);
    }
}

/**
 * Portals4 atomic operation.
 *
 * Execute Portals4 atomic function and return the old value
 * if requested.
 * @param[in] thread Destination thread
 * @param[in] doffset Destination offset
 * @param[in] fetch_ptr Fetch value pointer (optional)
 * @param[in] value Atomic value for the operation
 * @param[in] op Atomic operation
 * @param[in] type Atomic data type
 */
void
gupcr_atomic_op (size_t dthread, size_t doffset, void *fetch_ptr,
		 const void *value, ptl_op_t op, ptl_datatype_t type)
{
  ptl_process_t rpid;
  ptl_ct_event_t ct;
  char tmpbuf[128] __attribute__ ((unused));
  char atomic_tmp_buf[GUPC_MAX_ATOMIC_SIZE];
  size_t size = gupcr_get_atomic_size (type);
  gupcr_debug (FC_ATOMIC, "%lu:0x%lx %s:%s v(%s)", dthread, doffset,
	       gupcr_strptlop (op), gupcr_strptldatatype (type),
	       gupcr_get_buf_as_hex (tmpbuf, value, size));
  rpid.rank = dthread;
  if (fetch_ptr)
    {
      gupcr_portals_call (PtlFetchAtomic,
			  (gupcr_atomic_md, (ptl_size_t) atomic_tmp_buf,
			   gupcr_atomic_md, (ptl_size_t) value,
			   size, rpid, GUPCR_PTL_PTE_ATOMIC,
			   PTL_NO_MATCH_BITS, doffset,
			   PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA, op, type));
    }
  else
    {
      gupcr_portals_call (PtlAtomic,
			  (gupcr_atomic_md, (ptl_size_t) value,
			   size, PTL_ACK_REQ, rpid, GUPCR_PTL_PTE_ATOMIC,
			   PTL_NO_MATCH_BITS, doffset,
			   PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA, op, type));
    }
  gupcr_atomic_md_count += 1;
  gupcr_portals_call (PtlCTWait,
		      (gupcr_atomic_md_ct, gupcr_atomic_md_count, &ct));
  if (ct.failure)
    {
      gupcr_process_fail_events (gupcr_atomic_md_eq);
      gupcr_fatal_error ("received an error on atomic MD");
    }
  if (fetch_ptr)
    {
      gupcr_debug (FC_ATOMIC, "ov(%s)",
		   gupcr_get_buf_as_hex (tmpbuf, atomic_tmp_buf, size));
      memcpy (fetch_ptr, atomic_tmp_buf, size);
    }
}

/**
 * Initialize atomics resources.
 * @ingroup INIT
 */
void
gupcr_atomic_init (void)
{
  ptl_md_t md;

  gupcr_log (FC_ATOMIC, "atomic init called");

  /* Setup the Portals MD for local source/destination copying.
     We need to map the whole user's space (same as gmem).  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_atomic_md_ct));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_atomic_md_eq));
  md.length = (ptl_size_t) USER_PROG_MEM_SIZE;
  md.start = (void *) USER_PROG_MEM_START;
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_CT_REPLY |
    PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_atomic_md_eq;
  md.ct_handle = gupcr_atomic_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_atomic_md));

  /* Reset number of acknowledgments.  */
  gupcr_atomic_md_count = 0;
}

/**
 * Release atomics resources.
 * @ingroup INIT
 */
void
gupcr_atomic_fini (void)
{
  gupcr_log (FC_ATOMIC, "atomic fini called");
  /* Release atomic MD and its resources.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_atomic_md));
  gupcr_portals_call (PtlCTFree, (gupcr_atomic_md_ct));
  gupcr_portals_call (PtlEQFree, (gupcr_atomic_md_eq));
}

/** @} */
