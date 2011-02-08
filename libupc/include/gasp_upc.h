/* Copyright (c) 2009, 2010, 2011
   Free Software Foundation, Inc. 
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


#ifndef _GASP_UPC_H_
#define _GASP_UPC_H_

/* See GASP Specification (version 1.5)
   http://gasp.hcs.ufl.edu/gasp-1.5-61606.pdf  */

#define GASP_UPC_VERSION 40302

/* Opaque types */
typedef void gasp_upc_PTS_t;
typedef void gasp_upc_lock_t;
typedef void *gasp_upc_nb_handle_t;

/* Indicate that a non-blocking get/put shouldn't be trackeed.  */
#define GASP_NB_TRIVIAL ((gasp_upc_nb_handle_t) 0)

/* Reduction operation data types */
typedef enum
  {
    GASP_UPC_REDUCTION_C,
    GASP_UPC_REDUCTION_UC,
    GASP_UPC_REDUCTION_S,
    GASP_UPC_REDUCTION_US,
    GASP_UPC_REDUCTION_I,
    GASP_UPC_REDUCTION_UI,
    GASP_UPC_REDUCTION_L,
    GASP_UPC_REDUCTION_UL,
    GASP_UPC_REDUCTION_F,
    GASP_UPC_REDUCTION_D,
    GASP_UPC_REDUCTION_LD
  } gasp_upc_reduction_t;

/*

    GASP events defined in the specification document.

    Symbolic name               Event type  vararg arguments 
    =================================================
    GASP_C_FUNC                 Start,End   const char *funcsig 
    GASP_C_MALLOC               Start,End   size_t nbytes 
    GASP_C_REALLOC              Start,End   void *ptr, size_t size 
    GASP_C_FREE                 Start,End   void *ptr 
    GASP_UPC_COLLECTIVE_EXIT    Start,End   int status 
    GASP_UPC_NONCOLLECTIVE_EXIT Atomic      int status 
    GASP_UPC_NOTIFY             Start,End   int named, int expr 
    GASP_UPC_WAIT               Start,End   int named, int expr 
    GASP_UPC_BARRIER            Start,End   int named, int expr 
    GASP_UPC_FENCE              Start,End   (none) 
    GASP_UPC_FORALL             Start,End   (none) 
    GASP_UPC_GLOBAL_ALLOC       Start       size_t nblocks, size_t nbytes 
    GASP_UPC_GLOBAL_ALLOC       End         size_t nblocks, size_t nbytes,
                                            gasp_upc_PTS_t *newshrd_ptr 
    GASP_UPC_ALL_ALLOC          Start       size_t nblocks, size_t nbytes 
    GASP_UPC_ALL_ALLOC          End         size_t nblocks, size_t nbytes,
                                            gasp_upc_PTS_t *newshrd_ptr 
    GASP_UPC_ALLOC              Start       size_t nbytes 
    GASP_UPC_ALLOC              End         size_t nbytes,
                                            gasp_upc_PTS_t *newshrd_ptr 
    GASP_UPC_FREE               Start,End   gasp_upc_PTS_t *shrd_ptr 
    GASP_UPC_GLOBAL_LOCK_ALLOC  Start       (none) 
    GASP_UPC_GLOBAL_LOCK_ALLOC  End         gasp_upc_lock_t *lck 
    GASP_UPC_ALL_LOCK_ALLOC     Start       (none) 
    GASP_UPC_ALL_LOCK_ALLOC     End         gasp_upc_lock_t *lck 
    GASP_UPC_LOCK_FREE          Start,End   gasp_upc_lock_t *lck 
    GASP_UPC_LOCK               Start,End   gasp_upc_lock_t *lck 
    GASP_UPC_LOCK_ATTEMPT       Start       gasp_upc_lock_t *lck 
    GASP_UPC_LOCK_ATTEMPT       End         gasp_upc_lock_t *lck, int result 
    GASP_UPC_UNLOCK             Start,End   gasp_upc_lock_t *lck 
    GASP_UPC_MEMCPY             Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src, size_t n 
    GASP_UPC_MEMGET             Start,End   void *dst, gasp_upc_PTS_t *src,
                                            size_t n 
    GASP_UPC_MEMPUT             Start,End   gasp_upc_PTS_t *dst, void *src,
                                            size_t n 
    GASP_UPC_MEMSET             Start,End   gasp_upc_PTS_t *dst, int c,
                                            size_t n 
    GASP_UPC_GET                Start,End   int is_relaxed, void *dst, 
                                            gasp_upc_PTS_t *src, size_t n 
    GASP_UPC_PUT                Start,End   int is_relaxed,
                                            gasp_upc_PTS_t *dst, 
                                            void *src, size_t n 
    GASP_UPC_NB_GET_INIT        Start       int is_relaxed, void *dst,
                                            gasp_upc_PTS_t *src, size_t n 
    GASP_UPC_NB_GET_INIT        End         int is_relaxed, void *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t n,
					    gasp_upc_nb_handle_t handle 
    GASP_UPC_NB_GET_DATA        Start,End   gasp_upc_nb_handle_t handle 
    GASP_UPC_NB_PUT_INIT        Start       int is_relaxed, gasp_upc_PTS_t *dst,
                                            void *src, size_t n 
    GASP_UPC_NB_PUT_INIT        End         int is_relaxed, gasp_upc_PTS_t *dst,
                                            void *src, size_t n,
					    gasp_upc_nb_handle_t handle 
    GASP_UPC_NB_PUT_DATA        Start,End   gasp_upc_nb_handle_t handle 
    GASP_UPC_NB_SYNC            Start,End   gasp_upc_nb_handle_t handle
    GASP_UPC_CACHE_MISS         Atomic      size_t n, size_t n_lines 
    GASP_UPC_CACHE_HIT          Atomic      size_t n 
    GASP_UPC_CACHE_INVALIDATE   Atomic      size_t n_dirty 
    GASP_UPC_ALL_BROADCAST      Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t nbytes, int upc_flags 
    GASP_UPC_ALL_SCATTER        Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t nbytes, int upc_flags 
    GASP_UPC_ALL_GATHER         Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t nbytes, int upc_flags 
    GASP_UPC_ALL_GATHER_ALL     Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t nbytes, int upc_flags 
    GASP_UPC_ALL_EXCHANGE       Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            size_t nbytes, int upc_flags 
    GASP_UPC_ALL_PERMUTE        Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            gasp_upc_PTS_t *perm, size_t nbytes,
					    int upc_flags
    GASP_UPC_ALL_REDUCE         Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src,
                                            int upc_op, size_t nelems,
					    size_t blk_size,
					    void *func, int upc_flags,
					    gasp_upc_reduction_t type 
    GASP_UPC_ALL_PREFIX_REDUCE  Start,End   gasp_upc_PTS_t *dst,
                                            gasp_upc_PTS_t *src, int upc_op,
                                            size_t nelems, size_t blk_size,
					    void *func, int upc_flags,
					    gasp_upc_reduction_t type 
    GASP_UPC_USEREVT_START      First User-defined event
    GASP_UPC_USEREVT_END        Last User-defined event
*/

/* These might some day be set as configured items */
#undef GASP_C_MALLOC_SUPPORTED
#undef GASP_UPC_FENCE_SUPPORTED
#undef GASP_UPC_NB_SUPPORTED
#undef GASP_UPC_CACHE_SUPPORTED
#undef GASP_UPC_ALL_SUPPORTED

#define GASP_UPC_EVT_NONE      0
#define	GASP_C_FUNC	1
#ifdef GASP_C_MALLOC_SUPPORTED
    #define GASP_C_MALLOC 2
    #define GASP_C_REALLOC 3
    #define GASP_C_FREE 4
#endif /* GASP_C_MALLOC_SUPPORTED */
#define GASP_UPC_COLLECTIVE_EXIT 5
#define GASP_UPC_NONCOLLECTIVE_EXIT 6
#define GASP_UPC_NOTIFY 7
#define GASP_UPC_WAIT 8
#define GASP_UPC_BARRIER 9
#ifdef GASP_UPC_FENCE_SUPPORTED
    #define GASP_UPC_FENCE 10
#endif /* GASP_UPC_FENCE_SUPPORTED */
#define GASP_UPC_FORALL 11
#define GASP_UPC_GLOBAL_ALLOC 12
#define GASP_UPC_ALL_ALLOC 13
#define GASP_UPC_ALLOC 14
#define GASP_UPC_FREE 15
#define GASP_UPC_GLOBAL_LOCK_ALLOC 16
#define GASP_UPC_ALL_LOCK_ALLOC 17
#define GASP_UPC_LOCK_FREE 18
#define GASP_UPC_LOCK 19
#define GASP_UPC_LOCK_ATTEMPT 20
#define GASP_UPC_UNLOCK 21
#define GASP_UPC_MEMCPY 22
#define GASP_UPC_MEMGET 23
#define GASP_UPC_MEMPUT 24
#define GASP_UPC_MEMSET 25
#define GASP_UPC_GET 26
#define GASP_UPC_PUT 27
#ifdef GASP_UPC_NB_SUPPORTED
    #define GASP_UPC_NB_GET_INIT 28
    #define GASP_UPC_NB_GET_DATA 29
    #define GASP_UPC_NB_PUT_INIT 30
    #define GASP_UPC_NB_PUT_DATA 31
    #define GASP_UPC_NB_SYNC 32
#endif /* GASP_UPC_NB_SUPPORTED */
#if GASP_UPC_CACHE_SUPPORTED
    #define GASP_UPC_CACHE_MISS 33
    #define GASP_UPC_CACHE_HIT 34
    #define GASP_UPC_CACHE_INVALIDATE 35
#endif /* GASP_UPC_CACHE_SUPPORTED */
#ifdef GASP_UPC_ALL_SUPPORTED
    #define GASP_UPC_ALL_BROADCAST 36
    #define GASP_UPC_ALL_SCATTER 37
    #define GASP_UPC_ALL_GATHER 38
    #define GASP_UPC_ALL_GATHER_ALL 39
    #define GASP_UPC_ALL_EXCHANGE 40
    #define GASP_UPC_ALL_PERMUTE 41
    #define GASP_UPC_ALL_REDUCE 42
    #define GASP_UPC_ALL_PREFIX_REDUCE 43
#endif /* GASP_UPC_ALL_SUPPORTED */
#define GASP_UPC_USEREVT_START 1000
#define GASP_UPC_USEREVT_END   1999

#endif /* _GASP_UPC_H_ */
