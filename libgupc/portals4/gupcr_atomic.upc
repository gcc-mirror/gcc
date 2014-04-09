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

#include <upc.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <upc_atomic.h>
#include <portals4.h>
#include "gupcr_gmem.h"
#include "gupcr_utils.h"
#include "gupcr_atomic_sup.h"

/**
 * @file gupcr_atomic.upc
 * GUPC Portals4 UPC atomics implementation.
 *
 * All UPC atomic operations and data types, with exception of UPC_PTS,
 * are almost completely matched to the corresponding Portals4 atomics.
 * The following exceptions are made:
 *
 * UPC_SUB  Converted into Portals4 atomic add of a negative number.
 * UPC_INC  Converted into Portals4 atomic add of one.
 * UPC_DEC  Converted into Portals4 atomic add of negative one.
 *
 * UPC_PTS data type does not use Portals4 atomic operations (even though
 * 64 bit pointer-to-shared can fit into the int64 container).  This is
 * mainly due to the fact that pointer-to-shared comparison has to
 * disregard the phase part of the pointer and Portals4 does not have
 * support for CSWAP with a mask.
 */

/**
 * @addtogroup ATOMIC GUPCR Atomics Support Functions
 * @{
 */

/** Atomic domain representation */
struct upc_atomicdomain_struct
{
  upc_lock_t *lock;
  upc_op_t ops;
  upc_type_t type;
};

/**
 * Convert UPC to Portals4 atomic data type.
 *
 * @param [in] upc_type UPC atomic data type
 * @retval Portals4 atomic data type
 */
static inline ptl_datatype_t
gupcr_atomic_to_ptl_type (upc_type_t upc_type)
{
  switch (upc_type)
    {
    case UPC_INT:
      return UPC_ATOMIC_TO_PTL_INT;
    case UPC_UINT:
      return UPC_ATOMIC_TO_PTL_UINT;
    case UPC_LONG:
      return UPC_ATOMIC_TO_PTL_LONG;
    case UPC_ULONG:
      return UPC_ATOMIC_TO_PTL_ULONG;
    case UPC_INT32:
      return UPC_ATOMIC_TO_PTL_INT32;
    case UPC_UINT32:
      return UPC_ATOMIC_TO_PTL_UINT32;
    case UPC_INT64:
      return UPC_ATOMIC_TO_PTL_INT64;
    case UPC_UINT64:
      return UPC_ATOMIC_TO_PTL_UINT64;
    case UPC_FLOAT:
      return UPC_ATOMIC_TO_PTL_FLOAT;
    case UPC_DOUBLE:
      return UPC_ATOMIC_TO_PTL_DOUBLE;
    default:
      gupcr_error ("invalid UPC atomic type %d", (int) upc_type);
    }
  return -1;
}

/**
 * Convert UPC to Portals4 atomic operation.
 *
 * @param [in] upc_op UPC atomic operation
 * @retval Portals4 atomic operation
 */
static inline ptl_op_t
gupcr_atomic_to_ptl_op (upc_op_t upc_op)
{
  switch (upc_op)
    {
    case UPC_ADD:
      return PTL_SUM;
    case UPC_MULT:
      return PTL_PROD;
    case UPC_MAX:
      return PTL_MAX;
    case UPC_MIN:
      return PTL_MIN;
    case UPC_AND:
      return PTL_BAND;
    case UPC_OR:
      return PTL_BOR;
    case UPC_XOR:
      return PTL_BXOR;
    default:
      gupcr_error ("invalid UPC atomic op %d", (int) upc_op);
    }
  return -1;
}

/**
 * Convert UPC atomic operation into a string.
 *
 * @param [in] upc_op UPC atomic operation
 * @retval Character string
 */
static const char *
gupcr_get_atomic_op_as_string (upc_op_t upc_op)
{
  switch (upc_op)
    {
    case UPC_ADD:
      return "UPC_ADD";
    case UPC_AND:
      return "UPC_AND";
    case UPC_CSWAP:
      return "UPC_CSWAP";
    case UPC_DEC:
      return "UPC_DEC";
    case UPC_INC:
      return "UPC_INC";
    case UPC_GET:
      return "UPC_GET";
    case UPC_MAX:
      return "UPC_MAX";
    case UPC_MIN:
      return "UPC_MIN";
    case UPC_MULT:
      return "UPC_MULT";
    case UPC_OR:
      return "UPC_OR";
    case UPC_SET:
      return "UPC_SET";
    case UPC_SUB:
      return "UPC_SUB";
    case UPC_XOR:
      return "UPC_XOR";
    }
  return "UNKNOWN ATOMIC OP";
}

/**
 * Convert UPC atomic type into a string.
 *
 * @param [in] upc_type UPC atomic type
 * @retval Character string
 */
static const char *
gupcr_get_atomic_type_as_string (upc_type_t upc_type)
{
  switch (upc_type)
    {
    case UPC_INT:
      return "UPC_INT";
    case UPC_UINT:
      return "UPC_UINT";
    case UPC_LONG:
      return "UPC_LONG";
    case UPC_ULONG:
      return "UPC_ULONG";
    case UPC_INT32:
      return "UPC_INT32";
    case UPC_UINT32:
      return "UPC_UINT32";
    case UPC_INT64:
      return "UPC_INT64";
    case UPC_UINT64:
      return "UPC_UINT64";
    case UPC_FLOAT:
      return "UPC_FLOAT";
    case UPC_DOUBLE:
      return "UPC_DOUBLE";
    case UPC_PTS:
      return "UPC_PTS";
    }
  return "UNKNOWN ATOMIC TYPE";
}

/** Set value by UPC atomic type macro */
#define FUNC_TYPE_SET(__name__,__type__)    \
	*(__type__ *) buf = (__type__) value

/**
 * Set buffer to the value of the particular UPC atomic type.
 *
 * @param [in] buf Pointer to the buffer to set
 * @param [in] type UPC atomic type
 * @param [in] value Value to be set
 */
static void
gupcr_set_optype_val (void *buf, upc_type_t type, int value)
{
  switch (type)
    {
    case UPC_INT:
      FUNC_TYPE_SET (UPC_INT, int);
      break;
    case UPC_UINT:
      FUNC_TYPE_SET (UPC_UINT, unsigned int);
      break;
    case UPC_LONG:
      FUNC_TYPE_SET (UPC_LONG, long);
      break;
    case UPC_ULONG:
      FUNC_TYPE_SET (UPC_ULONG, unsigned long);
      break;
    case UPC_INT32:
      FUNC_TYPE_SET (UPC_INT32, int32_t);
      break;
    case UPC_UINT32:
      FUNC_TYPE_SET (UPC_UINT32, uint32_t);
      break;
    case UPC_INT64:
      FUNC_TYPE_SET (UPC_INT64, int64_t);
      break;
    case UPC_UINT64:
      FUNC_TYPE_SET (UPC_UINT64, uint64_t);
      break;
    case UPC_FLOAT:
      FUNC_TYPE_SET (UPC_FLOAT, float);
      break;
    case UPC_DOUBLE:
      FUNC_TYPE_SET (UPC_DOUBLE, double);
      break;
    default:
      gupcr_error ("wrong UPC type (%d)", type);
    }
}

/** Negate value by UPC atomic type macro */
#define FUNC_TYPE_NEGATE(__name__,__type__)    \
      *(__type__ *) dbuf = - *(__type__*) sbuf

/**
 * Negate value of the particular UPC atomic type.
 *
 * @param [in] dbuf Pointer to negated value
 * @param [in] sbuf Pointer to original value
 * @param [in] type UPC atomic type
 */
static void
gupcr_negate_atomic_type (void *dbuf, const void *sbuf, upc_type_t type)
{
  switch (type)
    {
    case UPC_INT:
      FUNC_TYPE_NEGATE (UPC_INT, int);
      break;
    case UPC_UINT:
      FUNC_TYPE_NEGATE (UPC_UINT, unsigned int);
      break;
    case UPC_LONG:
      FUNC_TYPE_NEGATE (UPC_LONG, long);
      break;
    case UPC_ULONG:
      FUNC_TYPE_NEGATE (UPC_ULONG, unsigned long);
      break;
    case UPC_INT32:
      FUNC_TYPE_NEGATE (UPC_INT32, int32_t);
      break;
    case UPC_UINT32:
      FUNC_TYPE_NEGATE (UPC_UINT32, uint32_t);
      break;
    case UPC_INT64:
      FUNC_TYPE_NEGATE (UPC_INT64, int64_t);
      break;
    case UPC_UINT64:
      FUNC_TYPE_NEGATE (UPC_UINT64, uint64_t);
      break;
    case UPC_FLOAT:
      FUNC_TYPE_NEGATE (UPC_FLOAT, float);
      break;
    case UPC_DOUBLE:
      FUNC_TYPE_NEGATE (UPC_DOUBLE, double);
      break;
    default:
      gupcr_error ("wrong UPC type (%d)", type);
    }
}

/** @} */

/**
 * @addtogroup UPCATOMIC UPC Atomics Functions
 * @{
 */

/**
 * UPC atomic relaxed operation.
 *
 * @param [in] domain Atomic domain
 * @param [in] fetch_ptr Target of the update
 * @param [in] op Atomic operation
 * @param [in] target Target address of the operation
 * @param [in] operand1 Operation required argument
 * @param [in] operand2 Operation required argument
 */
void
upc_atomic_relaxed (upc_atomicdomain_t * domain,
		    void *restrict fetch_ptr, upc_op_t op,
		    shared void *restrict target,
		    const void *restrict operand1,
		    const void *restrict operand2)
{
  struct upc_atomicdomain_struct *ldomain;
  char cvt_buf[GUPC_MAX_ATOMIC_SIZE];

  /* Complete all strict operations.  Portals4 runtime allows only
     outstanding put operations.  */
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();

  if (domain == NULL)
    gupcr_fatal_error ("NULL atomic domain pointer specified");

  ldomain = (struct upc_atomicdomain_struct *) &domain[MYTHREAD];

  gupcr_trace (FC_ATOMIC, "ATOMIC ENTER %s %s",
	       gupcr_get_atomic_op_as_string (op),
	       gupcr_get_atomic_type_as_string (ldomain->type));

  if (target == NULL)
    gupcr_fatal_error ("NULL atomic target pointer specified");

  if (!(op && ldomain->ops))
    {
      gupcr_fatal_error ("invalid operation (%s) for specified domain",
			 gupcr_get_atomic_op_as_string (op));
    }

  /* Check arguments.  */
  switch (op)
    {
    case UPC_GET:
      if (fetch_ptr == NULL)
	gupcr_fatal_error (
	  "atomic operation (UPC_GET) requires a non-NULL fetch pointer");
    case UPC_INC:
    case UPC_DEC:
      if (operand1 != NULL)
	gupcr_error ("atomic operation (%s) requires a NULL operand1",
		     gupcr_get_atomic_op_as_string (op));
      if (operand2 != NULL)
	gupcr_error ("atomic operation (%s) requires a NULL operand2",
		     gupcr_get_atomic_op_as_string (op));
      break;
    case UPC_CSWAP:
      if (operand1 == NULL)
	gupcr_fatal_error (
	  "atomic operation (UPC_CSWAP) requires a non-NULL operand1");
      if (operand2 == NULL)
	gupcr_fatal_error (
	  "atomic operation (UPC_CSWAP) requires a non-NULL operand2");
      break;
    default:
      if (operand1 == NULL)
	gupcr_fatal_error (
		"atomic operation (%s) requires a non-NULL operand1",
		gupcr_get_atomic_op_as_string (op));
      if (operand2 != NULL)
	gupcr_error ("atomic operation (%s) requires a NULL operand2",
		     gupcr_get_atomic_op_as_string (op));
    }

  /* UPC_PTS data type does not use Portals4 atomic operations,
     even though 64 bit pointer-to-shared fits in the int64
     container.  UPC_PTS supports only access operations (get, set, cswap)
     and as pointer compare needs to disregards the phase during
     comparison we are unable to place the pointer in some integral
     container (e.g. int64) and use Portals4 atomic ops.  */
  if (ldomain->type == UPC_PTS)
    {
      upc_lock (ldomain->lock);
      switch (op)
	{
	case UPC_GET:
	  *(shared void **) fetch_ptr = *(shared void *shared *) target;
	  break;
	case UPC_SET:
	  if (fetch_ptr)
	    *(shared void **) fetch_ptr = *(shared void *shared *) target;
	  *(shared void *shared *) target = *(shared void **) operand1;
	  break;
	case UPC_CSWAP:
	  {
	    shared void *tmp = *(shared void *shared *) target;
	    if (*(shared void **) operand1 == tmp)
	      *(shared void *shared *) target = *(shared void **) operand2;
	    if (fetch_ptr)
	      *(shared void **) fetch_ptr = tmp;
	  }
	  break;
	default:
	  upc_unlock (ldomain->lock);
	  gupcr_fatal_error ("invalid atomic operation (%s) for UPC_PTS",
			      gupcr_get_atomic_op_as_string (op));
	}
      upc_unlock (ldomain->lock);
    }
  else
    {
      size_t dthread = upc_threadof (target);
      size_t doffset = upc_addrfield (target);

      switch (op)
	{
	case UPC_GET:
	  gupcr_atomic_get (dthread, doffset, fetch_ptr,
			    gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_SET:
	  gupcr_atomic_set (dthread, doffset, fetch_ptr, operand1,
			    gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_CSWAP:
	  gupcr_atomic_cswap (dthread, doffset, fetch_ptr, operand1, operand2,
			      gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_AND:
	case UPC_OR:
	case UPC_XOR:
	  if (ldomain->type == UPC_PTS ||
	      ldomain->type == UPC_FLOAT || ldomain->type == UPC_DOUBLE)
	    {
	      gupcr_fatal_error (
			"invalid atomic operation (%s) for %s type",
			gupcr_get_atomic_op_as_string (op),
			gupcr_get_atomic_type_as_string (ldomain->type));
	    }
	  gupcr_atomic_op (dthread, doffset, fetch_ptr, operand1,
			   gupcr_atomic_to_ptl_op (op),
			   gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_ADD:
	case UPC_MULT:
	case UPC_MIN:
	case UPC_MAX:
	  gupcr_atomic_op (dthread, doffset, fetch_ptr,
			   operand1, gupcr_atomic_to_ptl_op (op),
			   gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_SUB:
	  /* As Portals4 does not have atomic subtract, UPC_SUB must be
	     converted into atomic add, UPC_ADD.  */
	  gupcr_negate_atomic_type (cvt_buf, operand1, ldomain->type);
	  gupcr_atomic_op (dthread, doffset, fetch_ptr,
			   cvt_buf, gupcr_atomic_to_ptl_op (UPC_ADD),
			   gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	case UPC_INC:
	case UPC_DEC:
	  if (op == UPC_INC)
	    gupcr_set_optype_val (cvt_buf, ldomain->type, 1);
	  else
	    gupcr_set_optype_val (cvt_buf, ldomain->type, -1);
	  gupcr_atomic_op (dthread, doffset, fetch_ptr, cvt_buf, PTL_SUM,
			   gupcr_atomic_to_ptl_type (ldomain->type));
	  break;
	default:
	  gupcr_fatal_error ("invalid atomic operation: %s",
			     gupcr_get_atomic_op_as_string (op));
	}
    }
  gupcr_trace (FC_ATOMIC, "ATOMIC EXIT");
}

/**
 * UPC atomic strict operation.
 *
 * @param [in] domain Atomic domain
 * @param [in] fetch_ptr Target of the update
 * @param [in] op Atomic operation
 * @param [in] target Target address of the operation
 * @param [in] operand1 Operation required argument
 * @param [in] operand2 Operation required argument
 */
void
upc_atomic_strict (upc_atomicdomain_t * domain,
		   void *restrict fetch_ptr,
		   upc_op_t op,
		   shared void *restrict target,
		   const void *restrict operand1,
		   const void *restrict operand2)
{
  upc_fence;
  upc_atomic_relaxed (domain, fetch_ptr, op, target, operand1, operand2);
  upc_fence;
}

/**
 * Collective allocation of atomic domain.
 *
 * Implementation uses native Portals4 atomic functions and the
 * hint field is ignored.
 *
 * @parm [in] type Atomic operation type
 * @parm [in] ops Atomic domain operations
 * @parm [in] hints Atomic operation hint
 * @retval Allocated atomic domain pointer
 */
upc_atomicdomain_t *
upc_all_atomicdomain_alloc (upc_type_t type,
			    upc_op_t ops,
			    upc_atomichint_t hints __attribute__ ((unused)))
{
  struct upc_atomicdomain_struct *ldomain;
  shared upc_atomicdomain_t *domain;

  gupcr_trace (FC_ATOMIC, "ATOMIC DOMAIN_ALLOC ENTER %s ops(%X)",
	       gupcr_get_atomic_type_as_string (type), (unsigned) ops);
  domain = (upc_atomicdomain_t *)
    upc_all_alloc (THREADS, sizeof (struct upc_atomicdomain_struct));
  gupcr_assert (domain != NULL);

  ldomain = (struct upc_atomicdomain_struct *) &domain[MYTHREAD];
  ldomain->lock = NULL;
  if (type == UPC_PTS)
    ldomain->lock = upc_all_lock_alloc ();
  ldomain->ops = ops;
  ldomain->type = type;
  gupcr_trace (FC_ATOMIC, "ATOMIC DOMAIN_ALLOC EXIT");
  return domain;
}

/**
 * Collective free of the atomic domain.
 *
 * @param [in] domain Pointer to atomic domain
 *
 * @ingroup UPCATOMIC UPC Atomic Functions
 */
void
upc_all_atomicdomain_free (upc_atomicdomain_t * domain)
{
  if (domain == NULL)
    gupcr_fatal_error ("NULL atomic domain pointer specified");
  upc_barrier;
  if (MYTHREAD == 0)
    {
      upc_lock_free (domain->lock);
      upc_free (domain);
    }
  upc_barrier;
}

/**
 * Query implementation for expected performance.
 *
 * @parm [in] ops Atomic domain operations
 * @parm [in] type Atomic operation type
 * @parm [in] addr Atomic address
 * @retval Expected performance
 */
int
upc_atomic_isfast (upc_type_t type __attribute__ ((unused)),
		   upc_op_t ops __attribute__ ((unused)),
		   shared void *addr __attribute__ ((unused)))
{
  if (type == UPC_PTS)
    return UPC_ATOMIC_PERFORMANCE_NOT_FAST;
  return UPC_ATOMIC_PERFORMANCE_FAST;
}

/** @} */
