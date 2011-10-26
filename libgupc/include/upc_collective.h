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


#ifndef _UPC_COLLECTIVE_H_
#define _UPC_COLLECTIVE_H_

/* Flag type for synchronization semantics
   (and potentially other uses) */

#define upc_flag_t		int

/* Synchronization flags */

#define UPC_IN_NOSYNC		1
#define UPC_IN_MYSYNC		2
#define UPC_IN_ALLSYNC		0
#define UPC_OUT_NOSYNC		4
#define UPC_OUT_MYSYNC		8
#define UPC_OUT_ALLSYNC		0

/* Operation type for upc_all_reduceT()
   and upc_all_prefix_reduceT() */

#define upc_op_t		int

#define UPC_ADD			0
#define UPC_MULT		1
#define UPC_AND			2
#define UPC_OR			3
#define UPC_XOR			4
#define UPC_LOGAND		5
#define UPC_LOGOR		6
#define UPC_MIN			7
#define UPC_MAX			8
#define UPC_FUNC		9
#define UPC_NONCOMM_FUNC	10

/* Function codes for error checking  */

#define UPC_BRDCST		0
#define UPC_SCAT		1
#define UPC_GATH		2
#define UPC_GATH_ALL		3
#define UPC_EXCH		4
#define UPC_PERM		5
#define UPC_RED			6
#define UPC_PRED		7
#define UPC_SORT		8

extern void upc_all_broadcast (shared void *dst,
			       shared const void *src,
			       size_t nbytes, upc_flag_t sync_mode);

extern void upc_coll_err (shared void *dst, shared const void *src,
			  shared const int *perm,
			  size_t nbytes,
			  upc_flag_t sync_mode,
			  size_t blk_size,
			  size_t nelems, upc_op_t op, upc_flag_t upc_coll_op);

extern void upc_all_exchange (shared void *dst, shared const void *src,
			      size_t nbytes, upc_flag_t sync_mode);

extern void upc_all_gather_all (shared void *dst,
				shared const void *src,
				size_t nbytes, upc_flag_t sync_mode);

extern void upc_all_gather (shared void *dst, shared const void *src,
			    size_t nbytes, upc_flag_t sync_mode);

extern void upc_coll_init ();

extern void upc_all_permute (shared void *dst, shared const void *src,
			     shared const int *perm, size_t nbytes,
			     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceC (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size,
				     signed char (*func) (signed char,
							  signed char),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceUC (shared void *dst,
				      shared const void *src, upc_op_t op,
				      size_t nelems, size_t blk_size,
				      unsigned char (*func) (unsigned char,
							     unsigned char),
				      upc_flag_t sync_mode);

extern void upc_all_prefix_reduceS (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size,
				     signed short (*func) (signed short,
							   signed short),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceUS (shared void *dst,
				      shared const void *src, upc_op_t op,
				      size_t nelems, size_t blk_size,
				      unsigned short (*func) (unsigned short,
							      unsigned short),
				      upc_flag_t sync_mode);

extern void upc_all_prefix_reduceI (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size,
				     signed int (*func) (signed int,
							 signed int),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceUI (shared void *dst,
				      shared const void *src, upc_op_t op,
				      size_t nelems, size_t blk_size,
				      unsigned int (*func) (unsigned int,
							    unsigned int),
				      upc_flag_t sync_mode);

extern void upc_all_prefix_reduceL (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size,
				     signed long (*func) (signed long,
							  signed long),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceUL (shared void *dst,
				      shared const void *src, upc_op_t op,
				      size_t nelems, size_t blk_size,
				      unsigned long (*func) (unsigned long,
							     unsigned long),
				      upc_flag_t sync_mode);

extern void upc_all_prefix_reduceF (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size, float (*func) (float,
								     float),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceD (shared void *dst, shared const void *src,
				     upc_op_t op, size_t nelems,
				     size_t blk_size, double (*func) (double,
								      double),
				     upc_flag_t sync_mode);

extern void upc_all_prefix_reduceLD (shared void *dst, shared const void *src,
				      upc_op_t op, size_t nelems,
				      size_t blk_size, long double (*func) (long double,
				 				            long double),
				      upc_flag_t sync_mode);

extern void upc_all_reduceC (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      signed char (*func) (signed char, signed char),
			      upc_flag_t sync_mode);

extern void upc_all_reduceUC (shared void *dst, shared const void *src,
			       upc_op_t op, size_t nelems, size_t blk_size,
			       unsigned char (*func) (unsigned char,
						      unsigned char),
			       upc_flag_t sync_mode);

extern void upc_all_reduceS (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      signed short (*func) (signed short,
						    signed short),
			      upc_flag_t sync_mode);

extern void upc_all_reduceUS (shared void *dst, shared const void *src,
			       upc_op_t op, size_t nelems, size_t blk_size,
			       unsigned short (*func) (unsigned short,
						       unsigned short),
			       upc_flag_t sync_mode);

extern void upc_all_reduceI (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      signed int (*func) (signed int, signed int),
			      upc_flag_t sync_mode);

extern void upc_all_reduceUI (shared void *dst, shared const void *src,
			       upc_op_t op, size_t nelems, size_t blk_size,
			       unsigned int (*func) (unsigned int,
						     unsigned int),
			       upc_flag_t sync_mode);

extern void upc_all_reduceL (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      signed long (*func) (signed long, signed long),
			      upc_flag_t sync_mode);

extern void upc_all_reduceUL (shared void *dst, shared const void *src,
			       upc_op_t op, size_t nelems, size_t blk_size,
			       unsigned long (*func) (unsigned long,
						      unsigned long),
			       upc_flag_t sync_mode);

extern void upc_all_reduceF (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      float (*func) (float, float),
			      upc_flag_t sync_mode);

extern void upc_all_reduceD (shared void *dst, shared const void *src,
			      upc_op_t op, size_t nelems, size_t blk_size,
			      double (*func) (double, double),
			      upc_flag_t sync_mode);

extern void upc_all_reduceLD (shared void *dst, shared const void *src,
			       upc_op_t op, size_t nelems, size_t blk_size,
			       long double (*func) (long double, long double),
			       upc_flag_t sync_mode);

extern void upc_all_scatter (shared void *dst, shared const void *src,
			     size_t nbytes, upc_flag_t sync_mode);

extern void upc_all_sort (shared void *A, size_t elem_size,
			  size_t nelems, size_t blk_size,
			  int (*func) (shared void *, shared void *),
			  upc_flag_t sync_mode);

#endif /* !_UPC_COLLECTIVE_H_ */
