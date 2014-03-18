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

/* UPC Atomic Memory Operations  */

#ifndef _UPC_ATOMIC_H_
#define _UPC_ATOMIC_H_

#include "upc_types.h"

/* Atomic operations not defined in <upc_types.h>.  */
#define  UPC_GET    (1UL<<9)
#define  UPC_SET    (1UL<<10)
#define  UPC_CSWAP  (1UL<<11)
#define  UPC_SUB    (1UL<<12)
#define  UPC_INC    (1UL<<13)
#define  UPC_DEC    (1UL<<14)

#define  UPC_GET_OP    9
#define  UPC_SET_OP    10
#define  UPC_CSWAP_OP  11
#define  UPC_SUB_OP    12
#define  UPC_INC_OP    13
#define  UPC_DEC_OP    14

/* Preferred mode of optimization of a domain.  */
typedef int upc_atomichint_t;
/* Preferred mode of optimization values.  */
#define UPC_ATOMIC_HINT_DEFAULT 0
#define UPC_ATOMIC_HINT_LATENCY 1
#define UPC_ATOMIC_HINT_THROUGHPUT 2

/* Atomics domain allocator (collective function).  */
upc_atomicdomain_t *upc_all_atomicdomain_alloc (upc_type_t type,
						upc_op_t ops,
						upc_atomichint_t hints);

/* Atomics domain release (collective function).  */
void upc_all_atomicdomain_free (upc_atomicdomain_t * ptr);

/* Atomics strict operation.  */
void upc_atomic_strict (upc_atomicdomain_t * domain,
			void *restrict fetch_ptr, upc_op_t op,
			shared void *restrict target,
			const void *restrict operand1,
			const void *restrict operand2);

/* Atomics relaxed operation.  */
void upc_atomic_relaxed (upc_atomicdomain_t * domain,
			 void *restrict fetch_ptr, upc_op_t op,
			 shared void *restrict target,
			 const void *restrict operand1,
			 const void *restrict operand2);

/* Atomics query function for expected performance.  */
int upc_atomic_isfast (upc_type_t type, upc_op_t ops, shared void *addr);
/* Expected performance return value.  */
#define UPC_ATOMIC_PERFORMANCE_NOT_FAST 0
#define UPC_ATOMIC_PERFORMANCE_FAST 1

#endif /* !_UPC_ATOMIC_H_ */
