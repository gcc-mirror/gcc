/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime library.
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

/*****************************************************************************/
/*                                                                           */
/*  Copyright (c) 2004, Michigan Technological University                    */
/*  All rights reserved.                                                     */
/*                                                                           */
/*  Redistribution and use in source and binary forms, with or without       */
/*  modification, are permitted provided that the following conditions       */
/*  are met:                                                                 */
/*                                                                           */
/*  * Redistributions of source code must retain the above copyright         */
/*  notice, this list of conditions and the following disclaimer.            */
/*  * Redistributions in binary form must reproduce the above                */
/*  copyright notice, this list of conditions and the following              */
/*  disclaimer in the documentation and/or other materials provided          */
/*  with the distribution.                                                   */
/*  * Neither the name of the Michigan Technological University              */
/*  nor the names of its contributors may be used to endorse or promote      */
/*  products derived from this software without specific prior written       */
/*  permission.                                                              */
/*                                                                           */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A  */
/*  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER */
/*  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, */
/*  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,      */
/*  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR       */
/*  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   */
/*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     */
/*  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             */
/*                                                                           */
/*****************************************************************************/

#include <stdlib.h>
#include <upc.h>
#include <upc_collective.h>
#include <upc_coll.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_coll_sup.h"

/*****************************************************************************/
/*                                                                           */
/*        UPC collective function library, reference implementation          */
/*                                                                           */
/*   Steve Seidel, Dept. of Computer Science, Michigan Technological Univ.   */
/*   steve@mtu.edu                                        March 1, 2004      */
/*                                                                           */
/*****************************************************************************/

/**
 * @file gupcr_coll_reduce.upc
 * GUPC Portals4 reduce collectives implementation.
 *
 * @addtogroup COLLECTIVES GUPCR Collectives Functions
 * @{
 */

/** Collectives reduce storage pointer */
gupcr_reduce_str_t gupcr_reduce_storage;

/**
 * Convert from UPC reduce to Portals atomic operation.
 *
 * @parm [in] op UPC reduce operation
 * @retval Portals atomic operation
*/
ptl_op_t
gupcr_portals_reduce_op (upc_op_t op)
{
  switch (op)
    {
    case UPC_ADD:
      return PTL_SUM;
    case UPC_MULT:
      return PTL_PROD;
    case UPC_AND:
      return PTL_BAND;
    case UPC_OR:
      return PTL_BOR;
    case UPC_XOR:
      return PTL_BXOR;
    case UPC_LOGAND:
      return PTL_LAND;
    case UPC_LOGOR:
      return PTL_LOR;
    case UPC_MIN:
      return PTL_MIN;
    case UPC_MAX:
      return PTL_MAX;
    default:
      gupcr_fatal_error ("cannot convert UPC reduce operation 0x%lx.", op);
    }
}



/**
 * Collectives reduce (C) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceC
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   signed char (*func) (signed char, signed char),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  signed char local_result = 0;
  signed char *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER signed char %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (signed char *) ((shared const signed char *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      signed char *t_result =
	(signed char *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(signed char *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed char),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_CHAR,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(signed char *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (signed char));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (signed char), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed char),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_CHAR, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(signed char *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (signed char));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (signed char),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_CHAR);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared signed char *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (UC) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceUC
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   unsigned char (*func) (unsigned char, unsigned char),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  unsigned char local_result = 0;
  unsigned char *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER unsigned char %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (unsigned char *) ((shared const unsigned char *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      unsigned char *t_result =
	(unsigned char *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(unsigned char *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned char),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_UCHAR,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(unsigned char *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (unsigned char));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (unsigned char), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned char),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_UCHAR, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(unsigned char *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (unsigned char));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (unsigned char),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_UCHAR);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared unsigned char *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (S) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceS
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   signed short (*func) (signed short, signed short),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  signed short local_result = 0;
  signed short *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER signed short %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (signed short *) ((shared const signed short *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      signed short *t_result =
	(signed short *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(signed short *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed short),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_SHORT,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(signed short *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (signed short));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (signed short), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed short),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_SHORT, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(signed short *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (signed short));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (signed short),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_SHORT);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared signed short *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (US) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceUS
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   unsigned short (*func) (unsigned short, unsigned short),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  unsigned short local_result = 0;
  unsigned short *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER unsigned short %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (unsigned short *) ((shared const unsigned short *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      unsigned short *t_result =
	(unsigned short *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(unsigned short *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned short),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_USHORT,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(unsigned short *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (unsigned short));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (unsigned short), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned short),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_USHORT, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(unsigned short *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (unsigned short));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (unsigned short),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_USHORT);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared unsigned short *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (I) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceI
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   signed int (*func) (signed int, signed int),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  signed int local_result = 0;
  signed int *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER signed int %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (signed int *) ((shared const signed int *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      signed int *t_result =
	(signed int *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(signed int *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed int),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_INT,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(signed int *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (signed int));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (signed int), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed int),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_INT, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(signed int *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (signed int));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (signed int),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_INT);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared signed int *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (UI) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceUI
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   unsigned int (*func) (unsigned int, unsigned int),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  unsigned int local_result = 0;
  unsigned int *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER unsigned int %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (unsigned int *) ((shared const unsigned int *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      unsigned int *t_result =
	(unsigned int *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(unsigned int *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned int),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_UINT,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(unsigned int *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (unsigned int));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (unsigned int), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned int),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_UINT, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(unsigned int *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (unsigned int));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (unsigned int),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_UINT);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared unsigned int *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (L) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceL
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   signed long (*func) (signed long, signed long),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  signed long local_result = 0;
  signed long *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER signed long %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (signed long *) ((shared const signed long *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      signed long *t_result =
	(signed long *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(signed long *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed long),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_LONG,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(signed long *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (signed long));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (signed long), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (signed long),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_LONG, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(signed long *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (signed long));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (signed long),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_LONG);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared signed long *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}


/**
 * Collectives reduce (UL) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceUL
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   unsigned long (*func) (unsigned long, unsigned long),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  unsigned long local_result = 0;
  unsigned long *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER unsigned long %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;


  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (unsigned long *) ((shared const unsigned long *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	  /* Skip if not integral type, per spec 4.3.1.1
	     (See additional comments in upc_collective.c) */
	case UPC_AND:
	  while (loop_cnt--)
	    local_result &= *l_src++;
	  break;
	case UPC_OR:
	  while (loop_cnt--)
	    local_result |= *l_src++;
	  break;
	case UPC_XOR:
	  while (loop_cnt--)
	    local_result ^= *l_src++;
	  break;
	case UPC_LOGAND:
	  while (loop_cnt--)
	    local_result = local_result && *l_src++;
	  break;
	case UPC_LOGOR:
	  while (loop_cnt--)
	    local_result = local_result || *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      unsigned long *t_result =
	(unsigned long *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(unsigned long *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned long),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_ULONG,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(unsigned long *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (unsigned long));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (unsigned long), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (unsigned long),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_ULONG, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(unsigned long *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (unsigned long));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (unsigned long),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_ULONG);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared unsigned long *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}

/**
 * Collectives UPC_LOGAND function for float types
 *
 * Portals4 does not define logical AND atomic operations
 * and they will be executed as functions.
 */
  float
gupcr_coll_logandF (float a, float b)
{
  return a && b;
}

/**
 * Collectives UPC_LOGOR function for float types
 *
 * Portals4 does not define logical OR atomic operations
 * and they will be executed as functions.
 */

float
gupcr_coll_logorF (float a, float b)
{
  return a || b;
}

/**
 * Collectives reduce (F) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceF
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   float (*func) (float, float),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  float local_result = 0;
  float *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER float %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;

  /* Logical operations on floating point types must execute as
     functions as Portals4 does not have support for them. */
  switch (op)
    {
    case UPC_LOGAND:
      func = &gupcr_coll_logandF;
      op = UPC_FUNC;
      break;
    case UPC_LOGOR:
      func = &gupcr_coll_logorF;
      op = UPC_FUNC;
      break;
    }

  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (float *) ((shared const float *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      float *t_result =
	(float *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(float *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (float),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_FLOAT,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(float *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (float));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (float), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (float),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_FLOAT, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(float *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (float));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (float),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_FLOAT);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared float *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}

/**
 * Collectives UPC_LOGAND function for float types
 *
 * Portals4 does not define logical AND atomic operations
 * and they will be executed as functions.
 */
  double
gupcr_coll_logandD (double a, double b)
{
  return a && b;
}

/**
 * Collectives UPC_LOGOR function for float types
 *
 * Portals4 does not define logical OR atomic operations
 * and they will be executed as functions.
 */

double
gupcr_coll_logorD (double a, double b)
{
  return a || b;
}

/**
 * Collectives reduce (D) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceD
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   double (*func) (double, double),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  double local_result = 0;
  double *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER double %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;

  /* Logical operations on floating point types must execute as
     functions as Portals4 does not have support for them. */
  switch (op)
    {
    case UPC_LOGAND:
      func = &gupcr_coll_logandD;
      op = UPC_FUNC;
      break;
    case UPC_LOGOR:
      func = &gupcr_coll_logorD;
      op = UPC_FUNC;
      break;
    }

  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (double *) ((shared const double *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      double *t_result =
	(double *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(double *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (double),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_DOUBLE,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(double *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (double));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (double), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (double),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_DOUBLE, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(double *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (double));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (double),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_DOUBLE);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared double *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}

/**
 * Collectives UPC_LOGAND function for float types
 *
 * Portals4 does not define logical AND atomic operations
 * and they will be executed as functions.
 */
  long double
gupcr_coll_logandLD (long double a, long double b)
{
  return a && b;
}

/**
 * Collectives UPC_LOGOR function for float types
 *
 * Portals4 does not define logical OR atomic operations
 * and they will be executed as functions.
 */

long double
gupcr_coll_logorLD (long double a, long double b)
{
  return a || b;
}

/**
 * Collectives reduce (LD) function
 *
 * The following steps are taken to calculate the reduced value:
 *
 * - Each thread reduces the values it has affinity to. Note that
 *   some of the threads might not participate in collectives reduce.
 * - A reduce tree is created out of the threads participating.
 * - All the parent threads signal their children that they are ready
 *   for the collectives reduce operation.
 * - All the children perform atomic portals reduce operations in the
 *   parent shared space. The reduced values are propagated to the
 *   top of the tree.
 * - Result is written to the specified destination.
 *
 * @param [in] dst Destination shared pointer
 * @param [in] src Source shared pointer
 * @param [in] op Collectives reduce operation
 * @param [in] nelems Number of elements
 * @param [in] blk_size Block size
 * @param [in] func Optional reduce function
 * @param [in] sync_mode Synchronization mode
 *
 */
void upc_all_reduceLD
  (shared void *dst,
   shared const void *src,
   upc_op_t op,
   size_t nelems,
   size_t blk_size,
   long double (*func) (long double, long double),
                       upc_flag_t sync_mode)
{
  int i, n_local, full_rows, last_row;
  int num_thr, tail_thr, extras, ph, src_thr, dst_thr, velems, start;

  long double local_result = 0;
  long double *l_src;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE ENTER long double %lu %lu",
               (long unsigned) nelems, (long unsigned) blk_size);

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))
    upc_barrier;

  /* Compute n_local, the number of elements local to this thread. */
  n_local = 0;

  /* Also compute start, the starting index of src for each thread. */

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  /* nelems plus the number of virtual elements in first row. */
  velems = nelems + src_thr * blk_size + ph;

  /* Include virtual elements when computing num of local elems. */
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  /* Calculate number of participating threads. */
  num_thr = (nelems + ph + blk_size - 1) / blk_size;
  if (num_thr > THREADS)
    num_thr = THREADS;

  gupcr_debug (FC_COLL,
	       "src_thr: %d tail_thr: %d ph: %d num_thr: %d full_rows: %d",
	       src_thr, tail_thr, ph, num_thr, full_rows);

  /* Calculate number of local elements. */
  if (blk_size > 0)
    {
      if (MYTHREAD <= tail_thr)
	if (MYTHREAD == tail_thr)
	  extras = last_row % blk_size;
	else
	  extras = blk_size;
      else
	extras = 0;

      n_local = blk_size * full_rows + extras;

      /* Adjust the number of elements in this thread, if necessary. */
      if (MYTHREAD < src_thr)
	n_local -= blk_size;
      else if (MYTHREAD == src_thr)
	n_local -= ph;
    }
  else				/* blk_size == 0 */
    {
      n_local = 0;
      if (src_thr == MYTHREAD)	/* revise the number of local elements */
	n_local = nelems;
    }

  /* Starting index for this thread
     Note: start is sometimes negative because src is
     addressed here as if its block size is 1. */

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			/* This is the source thread */
      start = 0;
  else				/* blk_size == 0 */
    start = 0;

  /* Logical operations on floating point types must execute as
     functions as Portals4 does not have support for them. */
  switch (op)
    {
    case UPC_LOGAND:
      func = &gupcr_coll_logandLD;
      op = UPC_FUNC;
      break;
    case UPC_LOGOR:
      func = &gupcr_coll_logorLD;
      op = UPC_FUNC;
      break;
    }

  /* Reduce the elements local to this thread. */

  if (n_local > 0)
    {
      int loop_cnt = n_local - 1;

      l_src = (long double *) ((shared const long double *) src + start);
      local_result = *l_src++;

      switch (op)
	{
	case UPC_ADD:
	  while (loop_cnt--)
	    local_result += *l_src++;
	  break;
	case UPC_MULT:
	  while (loop_cnt--)
	    local_result *= *l_src++;
	  break;
	case UPC_MIN:
	  while (loop_cnt--)
	    {
	      if (local_result > *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_MAX:
	  while (loop_cnt--)
	    {
	      if (local_result < *l_src)
		local_result = *l_src;
	      ++l_src;
	    }
	  break;
	case UPC_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	case UPC_NONCOMM_FUNC:
	  while (loop_cnt--)
	    local_result = func (local_result, *l_src++);
	  break;
	default:
	  gupcr_fatal_error ("bad UPC collectives reduce operator 0x%lx.", op);
	}
    }

  /* Note: local_result is undefined if n_local == 0.
     Note: Only a proper subset of threads might have a meaningful local_result
     Note: dst might be on a thread that does not have a local result */

  /* Global reduce on only participating threads. */
  if (n_local)
    {
      /* Local pointer where reduced values are written too. */
      long double *t_result =
	(long double *) & gupcr_reduce_storage[MYTHREAD].value[0];

      /* Initialize collectives reduce tree. */
      gupcr_coll_tree_setup (dst_thr, src_thr, num_thr);

      /* Copy in local results into the area for reduce operation.
         NOTE: Not needed for the case of collective functions. However,
         this covers the case of only one thread. */
      *t_result = local_result;

#ifdef GUPCR_USE_PORTALS4_TRIGGERED_OPS
/* Run reduce operation without triggered functions. */
#undef GUPCR_USE_PORTALS4_TRIGGERED_OPS
#endif
#if GUPCR_USE_PORTALS4_TRIGGERED_OPS
      /* Note: In the case of UPC_FUNC and UPC_NONCOMM, it is not possible
         to use triggered operations on inner nodes. In that case, inner
         nodes must calculate reduced value by calling the specified
         function. */
      if (gupcr_coll_child_cnt)
	{
	  if (IS_ROOT_THREAD)
	    {
	      /* ROOT THREAD */
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for children to report their values. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt);

	      /* Reduce local values with those of children if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result =
			func (local_result, *(long double *)
			      & gupcr_reduce_storage[MYTHREAD].value[i]);
		    }
		  *t_result = local_result;
		}
	    }
	  else
	    {
	      /* INNER THREAD */
	      /* Prepare triggered atomic function. */
	      if ((op != UPC_FUNC) && (op != UPC_NONCOMM_FUNC))
		{
		  /* Use triggered atomic operations once children sent
		     their results and parent is ready to receive it. */
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (long double),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_LONG_DOUBLE,
					     gupcr_coll_child_cnt + 1);
		}
	      /* Let children know that parent is ready. */
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].signal));
		  gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
		}
	      gupcr_coll_ack_wait (gupcr_coll_child_cnt);

	      /* Wait for completion - children reported and parent is ready. */
	      gupcr_coll_signal_wait (gupcr_coll_child_cnt + 1);
	      /* Execute reduce functions if necessary. */
	      if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
		{
		  size_t offset = upc_addrfield ((shared void *)
						 &(gupcr_reduce_storage
						   [MYTHREAD].value[0]));
		  size_t doffset =
		    upc_addrfield ((shared void *)
				   &(gupcr_reduce_storage[MYTHREAD].value
				     [gupcr_coll_child_index]));
		  /* Reduce local result with those of children. */
		  for (i = 0; i < gupcr_coll_child_cnt; i++)
		    {
		      local_result = func (local_result, *(long double *)
					   & gupcr_reduce_storage[MYTHREAD].
					   value[i]);
		    }
		  *t_result = local_result;
		  gupcr_coll_put (gupcr_coll_parent_thread, doffset, offset,
				  sizeof (long double));
		}
	      /* Wait for our value to go up the tree. */
	      gupcr_coll_ack_wait (1);
	    }
	}
      else
	{
	  /* Avoid the case where only one thread is available. */
	  if (!IS_ROOT_THREAD)
	    {
	      /* LEAF THREAD */
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      switch (op)
		{
		case UPC_FUNC:
		case UPC_NONCOMM_FUNC:
		  {
		    /* Schedule a triggered put once signal is received. */
		    size_t doffset = upc_addrfield ((shared void *)
						    &(gupcr_reduce_storage
						      [MYTHREAD].value
						      [gupcr_coll_child_index]));
		    gupcr_coll_trigput (gupcr_coll_parent_thread, doffset,
					offset, sizeof (long double), 1);
		  }
		  break;
		default:
		  /* Schedule a triggered atomic put once parent is ready. */
		  gupcr_coll_trigput_atomic (gupcr_coll_parent_thread, offset,
					     offset, sizeof (long double),
					     gupcr_portals_reduce_op (op),
					     UPC_COLL_TO_PTL_LONG_DOUBLE, 1);
		  break;
		}
	      /* Wait for parent to be ready. */
	      gupcr_coll_signal_wait (1);
	      /* Wait for our value to leave. */
	      gupcr_coll_ack_wait (1);
	    }
	}
#else /* NO TRIGGERED OPS */
      /* Send signal to all children. */
      if (gupcr_coll_child_cnt)
	{
	  /* ROOT OR INNER THREAD */
	  int wait_cnt = gupcr_coll_child_cnt;

	  /* Signal that parent is ready to receive the locally reduced
	     values from its children. Value that we send does not matter. */
	  for (i = 0; i < gupcr_coll_child_cnt; i++)
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       signal));
	      gupcr_coll_put (gupcr_coll_child[i], offset, offset, 1);
	    }
	  gupcr_coll_ack_wait (wait_cnt);

	  /* Wait for children to report their local reduced values and
	     parent to report it is ready to receive the reduced value. */
	  if (!IS_ROOT_THREAD)
	    ++wait_cnt;
	  gupcr_coll_signal_wait (wait_cnt);

	  /* Compute result if reduce functions are used. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      for (i = 0; i < gupcr_coll_child_cnt; i++)
		{
		  local_result = func (local_result,
				       *(long double *) &
				       gupcr_reduce_storage[MYTHREAD].
				       value[i]);
		}
	      /* Prepare reduced value for going up the tree. */
	      *t_result = local_result;
	    }
	}
      else if (!IS_ROOT_THREAD)
	{
	  /* LEAF THREAD */
	  gupcr_coll_signal_wait (1);
	}

      /* Send reduced value from the thread and its children to the parent. */
      if (!IS_ROOT_THREAD)
	{
	  /* LEAF OR INNER THREAD */
	  /* Each child places its result into the parent memory slot
	     dedicated for the child. The parent is responsible
	     for creating the reduced result for itself and its
	     children. */
	  if ((op == UPC_FUNC) || (op == UPC_NONCOMM_FUNC))
	    {
	      size_t doffset = upc_addrfield ((shared void *)
					      &(gupcr_reduce_storage
						[MYTHREAD].
						value
						[gupcr_coll_child_index]));
	      size_t soffset =
		upc_addrfield ((shared void *)
			       &(gupcr_reduce_storage[MYTHREAD].value[0]));
	      gupcr_coll_put (gupcr_coll_parent_thread, doffset, soffset,
			      sizeof (long double));
	    }
	  else
	    {
	      size_t offset = upc_addrfield ((shared void *)
					     &(gupcr_reduce_storage[MYTHREAD].
					       value[0]));
	      gupcr_coll_put_atomic (gupcr_coll_parent_thread, offset, offset,
				     sizeof (long double),
				     gupcr_portals_reduce_op (op),
				     UPC_COLL_TO_PTL_LONG_DOUBLE);
	    }
	  gupcr_coll_ack_wait (1);
	}
#endif /* GUPCR_USE_PORTALS4_TRIGGERED_OPS */

      /* Copy result into the caller's specified destination. */
      if (IS_ROOT_THREAD)
	{
	  *(shared long double *) dst = *t_result;
	}
    }

  /* Synchronize using barriers in the cases of MYSYNC and ALLSYNC. */
  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))
    upc_barrier;

  gupcr_trace (FC_COLL, "COLL ALL_REDUCE EXIT");
}
