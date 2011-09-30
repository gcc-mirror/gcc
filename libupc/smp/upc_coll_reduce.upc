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

/*****************************************************************************/
/*                                                                           */
/*        UPC collective function library, reference implementation          */
/*                                                                           */
/*   Steve Seidel, Dept. of Computer Science, Michigan Technological Univ.   */
/*   steve@mtu.edu                                        March 1, 2004      */
/*                                                                           */
/*****************************************************************************/

/* The true set of function names is in upc_all_collectives.c */


void upc_all_reduceC
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 signed char (*func) (signed char, signed char), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared signed char *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  signed char local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const signed char *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const signed char *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const signed char *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const signed char *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const signed char *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const signed char *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const signed char *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const signed char *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const signed char *) src + i))
	  local_result = *((shared const signed char *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const signed char *) src + i))
	  local_result = *((shared const signed char *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed char *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed char *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (signed char));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared signed char *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared signed char *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared signed char *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared signed char *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared signed char *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared signed char *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared signed char *) dst) =
	      *((shared signed char *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared signed char *) dst) =
	      *((shared signed char *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared signed char *) dst))
	      *((shared signed char *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared signed char *) dst))
	      *((shared signed char *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared signed char *) dst) =
	      func (local_result, *((shared signed char *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared signed char *) dst) =
	      func (local_result, *((shared signed char *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared signed char *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared signed char *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared signed char *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared signed char *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared signed char *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared signed char *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared signed char *) dst) =
		    *((shared signed char *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared signed char *) dst) =
		    *((shared signed char *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared signed char *) dst))
		    *((shared signed char *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared signed char *) dst))
		    *((shared signed char *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared signed char *) dst) = func (shared_result[i],
						       *((shared signed char *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared signed char *) dst) = func (shared_result[i],
						       *((shared signed char *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceUC
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 unsigned char (*func) (unsigned char, unsigned char), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared unsigned char *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  unsigned char local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const unsigned char *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const unsigned char *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const unsigned char *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const unsigned char *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const unsigned char *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const unsigned char *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const unsigned char *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const unsigned char *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const unsigned char *) src + i))
	  local_result = *((shared const unsigned char *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const unsigned char *) src + i))
	  local_result = *((shared const unsigned char *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned char *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned char *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (unsigned char));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared unsigned char *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared unsigned char *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared unsigned char *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared unsigned char *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared unsigned char *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared unsigned char *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared unsigned char *) dst) =
	      *((shared unsigned char *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared unsigned char *) dst) =
	      *((shared unsigned char *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared unsigned char *) dst))
	      *((shared unsigned char *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared unsigned char *) dst))
	      *((shared unsigned char *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared unsigned char *) dst) =
	      func (local_result, *((shared unsigned char *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared unsigned char *) dst) =
	      func (local_result, *((shared unsigned char *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared unsigned char *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared unsigned char *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared unsigned char *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared unsigned char *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared unsigned char *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared unsigned char *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared unsigned char *) dst) =
		    *((shared unsigned char *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared unsigned char *) dst) =
		    *((shared unsigned char *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared unsigned char *) dst))
		    *((shared unsigned char *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared unsigned char *) dst))
		    *((shared unsigned char *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared unsigned char *) dst) = func (shared_result[i],
						       *((shared unsigned char *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared unsigned char *) dst) = func (shared_result[i],
						       *((shared unsigned char *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceS
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 signed short (*func) (signed short, signed short), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared signed short *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  signed short local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const signed short *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const signed short *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const signed short *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const signed short *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const signed short *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const signed short *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const signed short *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const signed short *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const signed short *) src + i))
	  local_result = *((shared const signed short *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const signed short *) src + i))
	  local_result = *((shared const signed short *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed short *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed short *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (signed short));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared signed short *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared signed short *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared signed short *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared signed short *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared signed short *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared signed short *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared signed short *) dst) =
	      *((shared signed short *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared signed short *) dst) =
	      *((shared signed short *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared signed short *) dst))
	      *((shared signed short *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared signed short *) dst))
	      *((shared signed short *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared signed short *) dst) =
	      func (local_result, *((shared signed short *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared signed short *) dst) =
	      func (local_result, *((shared signed short *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared signed short *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared signed short *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared signed short *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared signed short *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared signed short *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared signed short *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared signed short *) dst) =
		    *((shared signed short *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared signed short *) dst) =
		    *((shared signed short *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared signed short *) dst))
		    *((shared signed short *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared signed short *) dst))
		    *((shared signed short *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared signed short *) dst) = func (shared_result[i],
						       *((shared signed short *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared signed short *) dst) = func (shared_result[i],
						       *((shared signed short *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceUS
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 unsigned short (*func) (unsigned short, unsigned short), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared unsigned short *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  unsigned short local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const unsigned short *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const unsigned short *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const unsigned short *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const unsigned short *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const unsigned short *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const unsigned short *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const unsigned short *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const unsigned short *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const unsigned short *) src + i))
	  local_result = *((shared const unsigned short *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const unsigned short *) src + i))
	  local_result = *((shared const unsigned short *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned short *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned short *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (unsigned short));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared unsigned short *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared unsigned short *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared unsigned short *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared unsigned short *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared unsigned short *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared unsigned short *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared unsigned short *) dst) =
	      *((shared unsigned short *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared unsigned short *) dst) =
	      *((shared unsigned short *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared unsigned short *) dst))
	      *((shared unsigned short *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared unsigned short *) dst))
	      *((shared unsigned short *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared unsigned short *) dst) =
	      func (local_result, *((shared unsigned short *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared unsigned short *) dst) =
	      func (local_result, *((shared unsigned short *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared unsigned short *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared unsigned short *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared unsigned short *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared unsigned short *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared unsigned short *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared unsigned short *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared unsigned short *) dst) =
		    *((shared unsigned short *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared unsigned short *) dst) =
		    *((shared unsigned short *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared unsigned short *) dst))
		    *((shared unsigned short *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared unsigned short *) dst))
		    *((shared unsigned short *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared unsigned short *) dst) = func (shared_result[i],
						       *((shared unsigned short *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared unsigned short *) dst) = func (shared_result[i],
						       *((shared unsigned short *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceI
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 signed int (*func) (signed int, signed int), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared signed int *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  signed int local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const signed int *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const signed int *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const signed int *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const signed int *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const signed int *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const signed int *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const signed int *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const signed int *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const signed int *) src + i))
	  local_result = *((shared const signed int *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const signed int *) src + i))
	  local_result = *((shared const signed int *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed int *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed int *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (signed int));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared signed int *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared signed int *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared signed int *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared signed int *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared signed int *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared signed int *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared signed int *) dst) =
	      *((shared signed int *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared signed int *) dst) =
	      *((shared signed int *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared signed int *) dst))
	      *((shared signed int *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared signed int *) dst))
	      *((shared signed int *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared signed int *) dst) =
	      func (local_result, *((shared signed int *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared signed int *) dst) =
	      func (local_result, *((shared signed int *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared signed int *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared signed int *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared signed int *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared signed int *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared signed int *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared signed int *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared signed int *) dst) =
		    *((shared signed int *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared signed int *) dst) =
		    *((shared signed int *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared signed int *) dst))
		    *((shared signed int *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared signed int *) dst))
		    *((shared signed int *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared signed int *) dst) = func (shared_result[i],
						       *((shared signed int *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared signed int *) dst) = func (shared_result[i],
						       *((shared signed int *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceUI
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 unsigned int (*func) (unsigned int, unsigned int), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared unsigned int *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  unsigned int local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const unsigned int *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const unsigned int *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const unsigned int *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const unsigned int *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const unsigned int *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const unsigned int *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const unsigned int *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const unsigned int *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const unsigned int *) src + i))
	  local_result = *((shared const unsigned int *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const unsigned int *) src + i))
	  local_result = *((shared const unsigned int *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned int *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned int *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (unsigned int));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared unsigned int *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared unsigned int *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared unsigned int *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared unsigned int *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared unsigned int *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared unsigned int *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared unsigned int *) dst) =
	      *((shared unsigned int *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared unsigned int *) dst) =
	      *((shared unsigned int *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared unsigned int *) dst))
	      *((shared unsigned int *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared unsigned int *) dst))
	      *((shared unsigned int *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared unsigned int *) dst) =
	      func (local_result, *((shared unsigned int *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared unsigned int *) dst) =
	      func (local_result, *((shared unsigned int *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared unsigned int *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared unsigned int *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared unsigned int *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared unsigned int *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared unsigned int *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared unsigned int *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared unsigned int *) dst) =
		    *((shared unsigned int *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared unsigned int *) dst) =
		    *((shared unsigned int *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared unsigned int *) dst))
		    *((shared unsigned int *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared unsigned int *) dst))
		    *((shared unsigned int *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared unsigned int *) dst) = func (shared_result[i],
						       *((shared unsigned int *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared unsigned int *) dst) = func (shared_result[i],
						       *((shared unsigned int *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceL
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 signed long (*func) (signed long, signed long), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared signed long *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  signed long local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const signed long *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const signed long *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const signed long *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const signed long *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const signed long *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const signed long *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const signed long *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const signed long *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const signed long *) src + i))
	  local_result = *((shared const signed long *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const signed long *) src + i))
	  local_result = *((shared const signed long *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed long *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const signed long *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (signed long));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared signed long *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared signed long *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared signed long *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared signed long *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared signed long *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared signed long *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared signed long *) dst) =
	      *((shared signed long *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared signed long *) dst) =
	      *((shared signed long *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared signed long *) dst))
	      *((shared signed long *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared signed long *) dst))
	      *((shared signed long *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared signed long *) dst) =
	      func (local_result, *((shared signed long *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared signed long *) dst) =
	      func (local_result, *((shared signed long *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared signed long *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared signed long *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared signed long *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared signed long *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared signed long *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared signed long *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared signed long *) dst) =
		    *((shared signed long *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared signed long *) dst) =
		    *((shared signed long *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared signed long *) dst))
		    *((shared signed long *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared signed long *) dst))
		    *((shared signed long *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared signed long *) dst) = func (shared_result[i],
						       *((shared signed long *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared signed long *) dst) = func (shared_result[i],
						       *((shared signed long *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceUL
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 unsigned long (*func) (unsigned long, unsigned long), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared unsigned long *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  unsigned long local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const unsigned long *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const unsigned long *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const unsigned long *) src + i);
	}
      break;

      // Skip if not integral type, per spec 4.3.1.1
      // (See additional comments in upc_collective.c)
    case UPC_AND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result &= *((shared const unsigned long *) src + i);
      break;

    case UPC_OR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result |= *((shared const unsigned long *) src + i);
      break;

    case UPC_XOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result ^= *((shared const unsigned long *) src + i);
      break;

    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const unsigned long *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const unsigned long *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const unsigned long *) src + i))
	  local_result = *((shared const unsigned long *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const unsigned long *) src + i))
	  local_result = *((shared const unsigned long *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned long *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const unsigned long *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (unsigned long));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared unsigned long *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared unsigned long *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared unsigned long *) dst) *= local_result;
	    break;
	    // Skip if not integral type, per spec 4.3.1.1
	    // (See additional comments in upc_collective.c)
	  case UPC_AND:
	    *((shared unsigned long *) dst) &= local_result;
	    break;
	  case UPC_OR:
	    *((shared unsigned long *) dst) |= local_result;
	    break;
	  case UPC_XOR:
	    *((shared unsigned long *) dst) ^= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared unsigned long *) dst) =
	      *((shared unsigned long *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared unsigned long *) dst) =
	      *((shared unsigned long *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared unsigned long *) dst))
	      *((shared unsigned long *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared unsigned long *) dst))
	      *((shared unsigned long *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared unsigned long *) dst) =
	      func (local_result, *((shared unsigned long *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared unsigned long *) dst) =
	      func (local_result, *((shared unsigned long *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared unsigned long *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared unsigned long *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared unsigned long *) dst) *= shared_result[i];
		  break;
		  // Skip if not integral type, per spec 4.3.1.1
		  // (See additional comments in upc_collective.c)
		case UPC_AND:
		  *((shared unsigned long *) dst) &= shared_result[i];
		  break;
		case UPC_OR:
		  *((shared unsigned long *) dst) |= shared_result[i];
		  break;
		case UPC_XOR:
		  *((shared unsigned long *) dst) ^= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared unsigned long *) dst) =
		    *((shared unsigned long *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared unsigned long *) dst) =
		    *((shared unsigned long *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared unsigned long *) dst))
		    *((shared unsigned long *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared unsigned long *) dst))
		    *((shared unsigned long *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared unsigned long *) dst) = func (shared_result[i],
						       *((shared unsigned long *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared unsigned long *) dst) = func (shared_result[i],
						       *((shared unsigned long *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceF
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 float (*func) (float, float), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared float *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  float local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const float *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const float *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const float *) src + i);
	}
      break;


    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const float *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const float *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const float *) src + i))
	  local_result = *((shared const float *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const float *) src + i))
	  local_result = *((shared const float *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const float *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const float *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (float));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared float *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared float *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared float *) dst) *= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared float *) dst) =
	      *((shared float *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared float *) dst) =
	      *((shared float *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared float *) dst))
	      *((shared float *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared float *) dst))
	      *((shared float *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared float *) dst) =
	      func (local_result, *((shared float *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared float *) dst) =
	      func (local_result, *((shared float *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared float *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared float *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared float *) dst) *= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared float *) dst) =
		    *((shared float *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared float *) dst) =
		    *((shared float *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared float *) dst))
		    *((shared float *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared float *) dst))
		    *((shared float *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared float *) dst) = func (shared_result[i],
						       *((shared float *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared float *) dst) = func (shared_result[i],
						       *((shared float *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceD
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 double (*func) (double, double), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared double *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  double local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const double *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const double *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const double *) src + i);
	}
      break;


    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const double *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const double *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const double *) src + i))
	  local_result = *((shared const double *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const double *) src + i))
	  local_result = *((shared const double *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const double *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const double *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (double));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared double *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared double *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared double *) dst) *= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared double *) dst) =
	      *((shared double *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared double *) dst) =
	      *((shared double *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared double *) dst))
	      *((shared double *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared double *) dst))
	      *((shared double *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared double *) dst) =
	      func (local_result, *((shared double *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared double *) dst) =
	      func (local_result, *((shared double *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared double *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared double *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared double *) dst) *= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared double *) dst) =
		    *((shared double *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared double *) dst) =
		    *((shared double *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared double *) dst))
		    *((shared double *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared double *) dst))
		    *((shared double *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared double *) dst) = func (shared_result[i],
						       *((shared double *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared double *) dst) = func (shared_result[i],
						       *((shared double *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}

void upc_all_reduceLD
(shared void *dst,
 shared const void *src,
 upc_op_t op,
 size_t nelems,
 size_t blk_size,
 long double (*func) (long double, long double), upc_flag_t sync_mode)
{

/*

Besides the optional, caller specified beginning and ending barriers,
this function contains one barrier separating the completion of the local
reductions in each thread and the cross-thread reduction of those results.

The PULL version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared vector and copy the local result to that vector.
	barrier
	The dst thread reduces the elements in the vector.

The PUSH version of upc_all_reduceT:

	Determine how many src elements are contained on each thread.
	Reduce the elements local to this thread.
	Allocate a shared lock and a shared counter to control access to dst.
	barrier
	Each thread contributes its local result to dst.
*/

// PULL is the default
#ifndef PULL
#ifndef PUSH
#define PULL TRUE
#endif
#endif

#ifdef PULL
  // pointer to shared vector of local results
  shared long double *shared_result;
#endif
#ifdef PUSH
  // lock for controlling access to dst
  upc_lock_t *dst_lock;
  shared int *dst_lock_cnt;
#endif

  int i,
    n_local,
    full_rows,
    last_row,
    num_thr, tail_thr, extras, ph, src_thr, dst_thr, thr, velems, start;

  int *elem_cnt_on_thr;

  long double local_result = 0;

  if (!upc_coll_init_flag)
    upc_coll_init ();

  if (blk_size == 0)
    blk_size = nelems;

#ifdef _UPC_COLL_CHECK_ARGS
  upc_coll_err (dst, src, NULL, 0, sync_mode, blk_size, nelems, op, UPC_RED);
#endif

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_IN_MYSYNC & sync_mode || !(UPC_IN_NOSYNC & sync_mode))

    upc_barrier;

  // Compute n_local, the number of elements local to this thread.
  // Also compute start, the starting index of src for each thread.

  src_thr = upc_threadof ((shared void *) src);
  dst_thr = upc_threadof ((shared void *) dst);
  ph = upc_phaseof ((shared void *) src);

  // We need to figure out which threads contain src elements.
  // The most complex case to deal with is the one in which only a proper
  // subset of threads contains src elements and the dst thread is not
  // among them.  Each thread can locally determine how many src
  // elements are on all threads so this is computed locally to
  // avoid an exchange of data among threads.

  elem_cnt_on_thr = (int *) malloc (THREADS * sizeof (int));

  // The number of threads with at least one local element.
  num_thr = 0;

  // nelems plus the number of virtual elements in first row
  velems = nelems + src_thr * blk_size + ph;

  // Include virtual elements when computing num of local elems
  full_rows = velems / (blk_size * THREADS);
  last_row = velems % (blk_size * THREADS);
  tail_thr = last_row / blk_size;

  for (thr = 0; thr < THREADS; ++thr)
    {
      if (blk_size > 0)
	{
	  if (thr <= tail_thr)
	    if (thr == tail_thr)
	      extras = last_row % blk_size;
	    else
	      extras = blk_size;
	  else
	    extras = 0;

	  n_local = blk_size * full_rows + extras;

	  // Adjust the number of elements in this thread, if necessary.
	  if (thr < src_thr)
	    n_local -= blk_size;
	  else if (thr == src_thr)
	    n_local -= ph;
	}
      else			// blk_size == 0
	{
	  n_local = 0;
	  if (src_thr == thr)	// revise the number of local elements
	    n_local = nelems;
	}

      if (n_local > 0)
	++num_thr;

      elem_cnt_on_thr[thr] = n_local;
    }

  n_local = elem_cnt_on_thr[MYTHREAD];

  // Starting index for this thread
  // Note: start is sometimes negative because src is
  // addressed here as if its block size is 1.

  if (blk_size > 0)
    if (MYTHREAD > src_thr)
      start = MYTHREAD - src_thr - ph * THREADS;
    else if (MYTHREAD < src_thr)
      start = (blk_size - ph) * THREADS + MYTHREAD - src_thr;
    else			// This is the source thread
      start = 0;
  else				// blk_size == 0
    start = 0;

  // Reduce the elements local to this thread.

  if (n_local > 0)
    local_result = *((shared const long double *) src + start);

  switch (op)
    {
    case UPC_ADD:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result += *((shared const long double *) src + i);
	}
      break;

    case UPC_MULT:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	{
	  local_result *= *((shared const long double *) src + i);
	}
      break;


    case UPC_LOGAND:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result && *((shared const long double *) src + i);
      break;

    case UPC_LOGOR:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = local_result || *((shared const long double *) src + i);
      break;

    case UPC_MIN:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result > *((shared const long double *) src + i))
	  local_result = *((shared const long double *) src + i);
      break;

    case UPC_MAX:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	if (local_result < *((shared const long double *) src + i))
	  local_result = *((shared const long double *) src + i);
      break;

    case UPC_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const long double *) src + i));
      break;

    case UPC_NONCOMM_FUNC:
      for (i = start + THREADS; i < (start + n_local * THREADS); i += THREADS)
	local_result = func (local_result,
			     *((shared const long double *) src + i));
      break;
    }

// Note: local_result is undefined if n_local == 0.
// Note: Only a proper subset of threads might have a meaningful local_result
// Note: dst might be on a thread that does not have a local result

#ifdef PULL
  // Allocate shared vector to store local results;
  shared_result = upc_all_alloc (THREADS, sizeof (long double));
  if (n_local > 0)
    shared_result[MYTHREAD] = local_result;

#endif // PULL

#ifdef PUSH
  dst_lock = upc_all_lock_alloc ();
  dst_lock_cnt = upc_all_alloc (1, sizeof (int));
  if (MYTHREAD == dst_thr)
    *dst_lock_cnt = 0;
#endif // PUSH

  // Make sure all threads are ready to combine their results.

  upc_barrier;

#ifdef PUSH

  if (n_local > 0)
    {
      upc_lock (dst_lock);

      if (*dst_lock_cnt == 0)
	// initialize dst
	*((shared long double *) dst) = local_result;
      else
	switch (op)
	  {
	  case UPC_ADD:
	    *((shared long double *) dst) += local_result;
	    break;
	  case UPC_MULT:
	    *((shared long double *) dst) *= local_result;
	    break;
	  case UPC_LOGAND:
	    *((shared long double *) dst) =
	      *((shared long double *) dst) && local_result;
	    break;
	  case UPC_LOGOR:
	    *((shared long double *) dst) =
	      *((shared long double *) dst) || local_result;
	    break;
	  case UPC_MIN:
	    if (local_result < *((shared long double *) dst))
	      *((shared long double *) dst) = local_result;
	    break;
	  case UPC_MAX:
	    if (local_result > *((shared long double *) dst))
	      *((shared long double *) dst) = local_result;
	    break;
	  case UPC_FUNC:
	    *((shared long double *) dst) =
	      func (local_result, *((shared long double *) dst));
	    break;
	  case UPC_NONCOMM_FUNC:
	    *((shared long double *) dst) =
	      func (local_result, *((shared long double *) dst));
	    break;
	  }			// else

      ++*dst_lock_cnt;

      if (*dst_lock_cnt == num_thr)
	{
	  upc_unlock (dst_lock);
	  upc_lock_free (dst_lock);
	  upc_free (dst_lock_cnt);
	}
      else
	upc_unlock (dst_lock);
    }

#endif // PUSH

#ifdef PULL
  if (MYTHREAD == dst_thr)
    {
      // initialize dst to the first non-null result
      i = 0;
      while (elem_cnt_on_thr[i] == 0)
	++i;

      *((shared long double *) dst) = shared_result[i];

      ++i;

      for (; i < THREADS; ++i)
	{
	  // Pull values only from threads where n_local>0.
	  if (elem_cnt_on_thr[i] > 0)
	    {
	      switch (op)
		{
		case UPC_ADD:
		  *((shared long double *) dst) += shared_result[i];
		  break;
		case UPC_MULT:
		  *((shared long double *) dst) *= shared_result[i];
		  break;
		case UPC_LOGAND:
		  *((shared long double *) dst) =
		    *((shared long double *) dst) && shared_result[i];
		  break;
		case UPC_LOGOR:
		  *((shared long double *) dst) =
		    *((shared long double *) dst) || shared_result[i];
		  break;
		case UPC_MIN:
		  if (shared_result[i] < *((shared long double *) dst))
		    *((shared long double *) dst) = shared_result[i];
		  break;
		case UPC_MAX:
		  if (shared_result[i] > *((shared long double *) dst))
		    *((shared long double *) dst) = shared_result[i];
		  break;
		case UPC_FUNC:
		  *((shared long double *) dst) = func (shared_result[i],
						       *((shared long double *)
							 dst));
		  break;
		case UPC_NONCOMM_FUNC:
		  *((shared long double *) dst) = func (shared_result[i],
						       *((shared long double *)
							 dst));
		  break;
		}
	    }
	}
      upc_free (shared_result);
    }
#endif // PULL

  free (elem_cnt_on_thr);

  // Synchronize using barriers in the cases of MYSYNC and ALLSYNC.

  if (UPC_OUT_MYSYNC & sync_mode || !(UPC_OUT_NOSYNC & sync_mode))

    upc_barrier;
}
