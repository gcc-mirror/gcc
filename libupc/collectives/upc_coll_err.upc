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

/*
   upc_coll_err.c is included if _UPC_COLL_CHECK_ARGS is defined.
   This code checks for the single-valuedness of collective function
   arguments as well as checking that all threads have called the
   same function.  Range checking of size_t arguments is done, and
   the perm array is checked to be a true permutation of 0..THREADS-1.
   However, (prefix) reduce functions of different types cannot be
   distinguished.  Overlapping src and dst arguments are not detected.

   When each thread reaches its collective call it fills in
   corresponding entries of the coll_arg_list defined below.
   This list has affinity to thread 0.  After a barrier,
   thread 0 does all the argument checking locally on this
   list.  If no errors are found, all threads are released to
   continue execution after a final barrier.
*/

#include <stdio.h>

typedef struct
{
  shared void *dst;
  shared const void *src;
  shared const int *perm;
  size_t nbytes;
  upc_flag_t sync_mode;
  size_t blk_size;
  size_t nelems;
  upc_op_t op;
  upc_flag_t upc_coll_op;
} coll_arg_list_type;

shared coll_arg_list_type *coll_arg_list;

static const char *
upc_coll_op_name (upc_flag_t upc_coll_op)
{
  switch (upc_coll_op)
    {
    case UPC_BRDCST:
      return "upc_all_broadcast";
      break;
    case UPC_SCAT:
      return "upc_all_scatter";
      break;
    case UPC_GATH:
      return "upc_all_gather";
      break;
    case UPC_GATH_ALL:
      return "upc_all_gather_all";
      break;
    case UPC_EXCH:
      return "upc_all_exchange";
      break;
    case UPC_PERM:
      return "upc_all_permute";
      break;
    case UPC_RED:
      return "upc_all_reduce";
      break;
    case UPC_PRED:
      return "upc_all_prefix_reduce";
      break;
    case UPC_SORT:
      return "upc_all_sort";
      break;
    }
  return NULL;
}

static void
upc_coll_chk_nbytes (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].nbytes != coll_arg_list[i].nbytes)
      {
	printf ("%s: nbytes must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (coll_arg_list[0].nbytes < 1)
    {
      printf ("%s: nbytes must be greater than 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_blk_size (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].blk_size != coll_arg_list[i].blk_size)
      {
	printf ("%s: blk_size must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }
  // Note: blk_size is an unsigned int so we do not check whether
  // it might be negative.
}

static void
upc_coll_chk_nelems (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].nelems != coll_arg_list[i].nelems)
      {
	printf ("%s: nelems must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (coll_arg_list[0].nelems < 1)
    {
      printf ("%s: nelems must be greater than 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_elemsize (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].nbytes != coll_arg_list[i].nbytes)
      {
	printf ("%s: elem_size must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (coll_arg_list[0].nbytes < 1)
    {
      printf ("%s: elem_size must be greater than 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_dst_affinity (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].dst != coll_arg_list[i].dst)
      {
	printf ("%s: dst must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (upc_threadof (coll_arg_list[0].dst) != 0)
    {
      printf ("%s: Target of dst pointer must have affinity to thread 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_src_affinity (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].src != coll_arg_list[i].src)
      {
	printf ("%s: src must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (upc_threadof ((shared void *) coll_arg_list[0].src) != 0)
    {
      printf ("%s: Target of src pointer must have affinity to thread 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_phase (void)
{
  if (upc_phaseof ((shared void *) coll_arg_list[0].src)
      != upc_phaseof ((shared void *) coll_arg_list[0].dst))
    {
      printf
	("%s: Implementation limitation - src and dst must have the same phase.\n",
	 upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }
}

static void
upc_coll_chk_perm (void)
{
  int i, j;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].perm != coll_arg_list[i].perm)
      {
	printf ("%s: perm must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  if (upc_threadof ((shared void *) coll_arg_list[0].perm) != 0)
    {
      printf ("%s: Target of perm pointer must have affinity to thread 0\n",
	      upc_coll_op_name (coll_arg_list[0].upc_coll_op));
      upc_global_exit (1);
    }

  // Check that perm[0..THREADS-1] contains a permutation of 0..THREADS-1.

  for (i = 0; i < THREADS; i++)
    {
      for (j = 0; j < THREADS; j++)
	if (i == (coll_arg_list[0].perm)[j])
	  break;
      if (j == THREADS)
	{
	  printf ("%s: Permutation array must contain a"
		  " permutation of 0..THREADS-1. \n",
		  upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	  upc_global_exit (1);
	}
    }
}

static void
upc_coll_chk_op (void)
{
  int i;

  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].op != coll_arg_list[i].op)
      {
	printf ("%s: op must have the same value in corresponding"
		" calls to collective function\n",
		upc_coll_op_name (coll_arg_list[0].upc_coll_op));
	upc_global_exit (1);
      }

  for (i = 0; i < THREADS; ++i)
    if (coll_arg_list[i].op < 0 || coll_arg_list[i].op > 10)
      {
	printf ("%s: Illegal operation specified in thread %d.\n",
		upc_coll_op_name (coll_arg_list[i].upc_coll_op), i);
	upc_global_exit (1);
      }
}

static void
upc_coll_chk_sync_mode (void)
{
  int i;

  // Check for nonsensical combinations of synchronization modes
  // and for meaningless values of sync_mode.

  for (i = 0; i < THREADS; ++i)
    {
      upc_flag_t in_mode = coll_arg_list[i].sync_mode &
	(UPC_IN_NOSYNC | UPC_IN_MYSYNC | UPC_IN_ALLSYNC);
      upc_flag_t out_mode = coll_arg_list[i].sync_mode &
	(UPC_OUT_NOSYNC | UPC_OUT_MYSYNC | UPC_OUT_ALLSYNC);
      if (((in_mode != 0) && (in_mode != UPC_IN_ALLSYNC) &&
	   (in_mode != UPC_IN_MYSYNC) && (in_mode != UPC_IN_NOSYNC)) ||
	  ((out_mode != 0) && (out_mode != UPC_OUT_ALLSYNC) &&
	   (out_mode != UPC_OUT_MYSYNC) && (out_mode != UPC_OUT_NOSYNC)) ||
	  ((in_mode | out_mode) != coll_arg_list[i].sync_mode))

	{
	  printf ("%s: Conflicting or unknown collective synchronization"
		  " modes in thread %d. \n",
		  upc_coll_op_name (coll_arg_list[i].upc_coll_op), i);
	  upc_global_exit (1);
	}
    }
  // Check for conflicting synchronization modes.
  for (i = 1; i < THREADS; ++i)
    if (coll_arg_list[0].sync_mode != coll_arg_list[i].sync_mode)
      {
	printf ("%s: Threads 0 and %d have two different synchronization"
		" modes\n", upc_coll_op_name (coll_arg_list[i].upc_coll_op), i);
	upc_global_exit (1);
      }
}

void
upc_coll_err (shared void *dst,
	      shared const void *src,
	      shared const int *perm,
	      size_t nbytes,
	      upc_flag_t sync_mode,
	      size_t blk_size,
	      size_t nelems, upc_op_t op, upc_flag_t upc_coll_op)
{
  int i;

  coll_arg_list = upc_all_alloc (THREADS, sizeof (coll_arg_list_type));

  coll_arg_list[MYTHREAD].dst = dst;
  coll_arg_list[MYTHREAD].src = src;
  coll_arg_list[MYTHREAD].perm = perm;
  coll_arg_list[MYTHREAD].nbytes = nbytes;
  coll_arg_list[MYTHREAD].sync_mode = sync_mode;
  coll_arg_list[MYTHREAD].blk_size = blk_size;
  coll_arg_list[MYTHREAD].nelems = nelems;
  coll_arg_list[MYTHREAD].op = op;
  coll_arg_list[MYTHREAD].upc_coll_op = upc_coll_op;

  upc_barrier;

  // Now thread 0 has access to the arguments of all of the collective
  // function calls.  All other threads now wait until thread 0 checks
  // the arguments to all threads' collective function.

  if (MYTHREAD == 0)

    {
      // Check that all collective calls are to the same function.
      // (At most one disagreement is detected.)
      // However, mismatches between (prefix) reduce functions
      // of different types is not detected.

      for (i = 1; i < THREADS; ++i)
	if (coll_arg_list[0].upc_coll_op != coll_arg_list[i].upc_coll_op)
	  {
	    printf ("Threads 0 and %d have "
		    "called two different collective "
		    "functions: %s \t %s \n",
		    i, upc_coll_op_name (coll_arg_list[0].upc_coll_op),
		    upc_coll_op_name (coll_arg_list[i].upc_coll_op));
	    upc_global_exit (1);
	  }

      switch (upc_coll_op)
	{
	case UPC_BRDCST:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_dst_affinity ();
	  break;
	case UPC_SCAT:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_dst_affinity ();
	  break;
	case UPC_GATH:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_src_affinity ();
	  break;
	case UPC_GATH_ALL:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_src_affinity ();
	  upc_coll_chk_dst_affinity ();
	  break;
	case UPC_EXCH:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_src_affinity ();
	  upc_coll_chk_dst_affinity ();
	  break;
	case UPC_PERM:
	  upc_coll_chk_perm ();
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_nbytes ();
	  upc_coll_chk_src_affinity ();
	  upc_coll_chk_dst_affinity ();
	  break;
	case UPC_RED:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_op ();
	  upc_coll_chk_blk_size ();
	  upc_coll_chk_nelems ();
	  break;
	case UPC_PRED:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_op ();
	  upc_coll_chk_blk_size ();
	  upc_coll_chk_nelems ();
	  upc_coll_chk_phase ();
	  break;
	case UPC_SORT:
	  upc_coll_chk_sync_mode ();
	  upc_coll_chk_blk_size ();
	  upc_coll_chk_nelems ();
	  // use  nbytes instead of the id elem_size as in spec
	  upc_coll_chk_elemsize ();
	  break;
	}			// switch
      upc_free (coll_arg_list);
    }
  // If thread 0 detected no errors, all threads are released to
  // execute their collective functions.

  upc_barrier;
}
