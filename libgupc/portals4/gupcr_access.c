/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
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
#include "gupcr_access.h"
#include "gupcr_sync.h"
#include "gupcr_sup.h"
#include "gupcr_portals.h"
#include "gupcr_node.h"
#include "gupcr_gmem.h"
#include "gupcr_utils.h"

/**
 * @file gupcr_access.c
 * GUPC compiler access functions.
 */

/**
 * @addtogroup IFACE GUPC Interface Routines
 * @{
 */

//begin lib_inline_access

/**
 * Relaxed shared "char (8 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Char (8 bits) value at the shared address given by 'p'.
 */
//inline
u_intQI_t
__getqi2 (upc_shared_ptr_t p)
{
  u_intQI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R QI LOCAL");
      result = *(u_intQI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R QI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "short (16 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Short (16 bits) value at the shared address given by 'p'.
 */
//inline
u_intHI_t
__gethi2 (upc_shared_ptr_t p)
{
  u_intHI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R HI LOCAL");
      result = *(u_intHI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R HI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "int (32 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Int (32 bits) value at the shared address given by 'p'.
 */
//inline
u_intSI_t
__getsi2 (upc_shared_ptr_t p)
{
  u_intSI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R SI LOCAL");
      result = *(u_intSI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R SI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "long (64 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long (64 bits) value at the shared address given by 'p'.
 */
//inline
u_intDI_t
__getdi2 (upc_shared_ptr_t p)
{
  u_intDI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R DI LOCAL");
      result = *(u_intDI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R DI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%llx",
	       thread, (long unsigned) offset, (long long unsigned) result);
  return result;
}

#if GUPCR_TARGET64
/**
 * Relaxed shared "long long (128 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long long (128 bits) value at the shared address given by 'p'.
 */
//inline
u_intTI_t
__getti2 (upc_shared_ptr_t p)
{
  u_intTI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R TI LOCAL");
      result = *(u_intTI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R TI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%llx",
	       thread, (long unsigned) offset, (long long unsigned) result);
  return result;
}
#endif /* GUPCR_TARGET64 */
/**
 * Relaxed shared "float" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Float value at the shared address given by 'p'.
 */
//inline
float
__getsf2 (upc_shared_ptr_t p)
{
  float result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R SF LOCAL");
      result = *(float *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R SF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6g",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Double value at the shared address given by 'p'.
 */
//inline
double
__getdf2 (upc_shared_ptr_t p)
{
  double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R DF LOCAL");
      result = *(double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R DF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6g",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "long double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long double value at the shared address given by 'p'.
 */
//inline
long double
__gettf2 (upc_shared_ptr_t p)
{
  long double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R TF LOCAL");
      result = *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R TF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6Lg",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared "long double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long double value at the shared address given by 'p'.
 */
//inline
long double
__getxf2 (upc_shared_ptr_t p)
{
  long double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER R XF LOCAL");
      result = *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER R XF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6Lg",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Relaxed shared memory block get operation.
 * Copy the data at the shared address 'src' into the local memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Local address of the destination memory block.
 * @param [in] src Shared address of the source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__getblk3 (void *dest, upc_shared_ptr_t src, size_t n)
{
  int thread = GUPCR_PTS_THREAD (src);
  size_t offset = GUPCR_PTS_OFFSET (src);
  gupcr_trace (FC_MEM, "GETBLK ENTER R");
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      GUPCR_MEM_BARRIER ();
      memcpy (dest, GUPCR_GMEM_OFF_TO_LOCAL (thread, offset), n);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_gmem_get (dest, thread, offset, n);
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GETBLK EXIT R %d:0x%lx 0x%lx %lu",
	       thread, (long unsigned) offset,
	       (long unsigned) dest, (long unsigned) n);
}

/**
 * Relaxed shared "char (8 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putqi2 (upc_shared_ptr_t p, u_intQI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R QI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      *(u_intQI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R QI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R QI");
}

/**
 * Relaxed shared "short (16 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__puthi2 (upc_shared_ptr_t p, u_intHI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R HI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      *(u_intHI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R HI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R HI");
}

/**
 * Relaxed shared "int (32 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsi2 (upc_shared_ptr_t p, u_intSI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R SI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      *(u_intSI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R SI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R SI");
}

/**
 * Relaxed shared "long (64 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putdi2 (upc_shared_ptr_t p, u_intDI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R DI LOCAL "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      *(u_intDI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R DI REMOTE "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R DI");
}

#if GUPCR_TARGET64
/**
 * Relaxed shared "long long (128 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putti2 (upc_shared_ptr_t p, u_intTI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R TI LOCAL "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      *(u_intTI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R TI REMOTE "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R TI");
}
#endif /* GUPCR_TARGET64 */
/**
 * Relaxed shared "float" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsf2 (upc_shared_ptr_t p, float v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R SF LOCAL "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      *(float *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R SF REMOTE "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R SF");
}

/**
 * Relaxed shared "double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putdf2 (upc_shared_ptr_t p, double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R DF LOCAL "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      *(double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R DF REMOTE "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R DF");
}

/**
 * Relaxed shared "long double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__puttf2 (upc_shared_ptr_t p, long double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R TF LOCAL "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R TF REMOTE "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R TF");
}

/**
 * Relaxed shared "long double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putxf2 (upc_shared_ptr_t p, long double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER R XF LOCAL "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER R XF REMOTE "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	  /* There can be only one outstanding unordered put.  */
	  gupcr_pending_strict_put = 1;
	}
    }
  gupcr_trace (FC_MEM, "PUT EXIT R XF");
}

/**
 * Relaxed shared memory block put operation.
 * Copy the data at the local address 'src' into the shared memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Shared address of the destination memory block.
 * @param [in] src Local address of the source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__putblk3 (upc_shared_ptr_t dest, void *src, size_t n)
{
  int thread = GUPCR_PTS_THREAD (dest);
  size_t offset = GUPCR_PTS_OFFSET (dest);
  gupcr_trace (FC_MEM, "PUTBLK ENTER R 0x%lx %d:0x%lx %lu",
	       (long unsigned) src, thread,
	       (long unsigned) offset, (long unsigned) n);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      memcpy (GUPCR_GMEM_OFF_TO_LOCAL (thread, offset), src, n);
    }
  else
    {
      gupcr_gmem_put (thread, offset, src, n);
    }
  gupcr_trace (FC_MEM, "PUT_BLK EXIT R");
}

/**
 * Relaxed shared memory block copy operation.
 * Copy the data at the shared address 'src' into the shared memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Shared address of destination memory block.
 * @param [in] src Shared address of source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__copyblk3 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n)
{
  int dthread = GUPCR_PTS_THREAD (dest);
  size_t doffset = GUPCR_PTS_OFFSET (dest);
  int sthread = GUPCR_PTS_THREAD (src);
  size_t soffset = GUPCR_PTS_OFFSET (src);
  gupcr_trace (FC_MEM, "COPYBLK ENTER R %d:0x%lx %d:0x%lx %lu",
	       sthread, (long unsigned) soffset,
	       dthread, (long unsigned) doffset, (long unsigned) n);
  gupcr_assert (dthread < THREADS);
  gupcr_assert (doffset != 0);
  gupcr_assert (sthread < THREADS);
  gupcr_assert (soffset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (dthread) && GUPCR_GMEM_IS_LOCAL (sthread))
    {
      memcpy (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
	      GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
    }
  else if (GUPCR_GMEM_IS_LOCAL (dthread))
    {
      gupcr_gmem_get (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
		      sthread, soffset, n);
      gupcr_gmem_sync_gets ();
    }
  else if (GUPCR_GMEM_IS_LOCAL (sthread))
    {
      gupcr_gmem_put (dthread, doffset,
		      GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
    }
  else
    {
      gupcr_gmem_copy (dthread, doffset, sthread, soffset, n);
    }
  gupcr_trace (FC_MEM, "COPY_BLK EXIT R");
}

/**
 * Strict shared "char (8 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Char (8 bits) value at the shared address given by 'p'.
 */
//inline
u_intQI_t
__getsqi2 (upc_shared_ptr_t p)
{
  u_intQI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S QI LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(u_intQI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S QI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "short (16 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Short (16 bits) value at the shared address given by 'p'.
 */
//inline
u_intHI_t
__getshi2 (upc_shared_ptr_t p)
{
  u_intHI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S HI LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(u_intHI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S HI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "int (32 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Int (32 bits) value at the shared address given by 'p'.
 */
//inline
u_intSI_t
__getssi2 (upc_shared_ptr_t p)
{
  u_intSI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S SI LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(u_intSI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S SI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%x",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "long (64 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long (64 bits) value at the shared address given by 'p'.
 */
//inline
u_intDI_t
__getsdi2 (upc_shared_ptr_t p)
{
  u_intDI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S DI LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(u_intDI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S DI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%llx",
	       thread, (long unsigned) offset, (long long unsigned) result);
  return result;
}

#if GUPCR_TARGET64
/**
 * Strict shared "long long (128 bits)" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long long (128 bits) value at the shared address given by 'p'.
 */
//inline
u_intTI_t
__getsti2 (upc_shared_ptr_t p)
{
  u_intTI_t result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S TI LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(u_intTI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S TI REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx 0x%llx",
	       thread, (long unsigned) offset, (long long unsigned) result);
  return result;
}
#endif /* GUPCR_TARGET64 */
/**
 * Strict shared "float" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Float value at the shared address given by 'p'.
 */
//inline
float
__getssf2 (upc_shared_ptr_t p)
{
  float result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S SF LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(float *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S SF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6g",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Double value at the shared address given by 'p'.
 */
//inline
double
__getsdf2 (upc_shared_ptr_t p)
{
  double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S DF LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S DF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6g",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "long double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long double value at the shared address given by 'p'.
 */
//inline
long double
__getstf2 (upc_shared_ptr_t p)
{
  long double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S TF LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S TF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6Lg",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared "long double" get operation.
 * Return the value at the shared address 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the source operand.
 * @return Long double value at the shared address given by 'p'.
 */
//inline
long double
__getsxf2 (upc_shared_ptr_t p)
{
  long double result;
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "GET ENTER S XF LOCAL");
      GUPCR_MEM_BARRIER ();
      result = *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "GET ENTER S XF REMOTE");
      gupcr_gmem_get (&result, thread, offset, sizeof (result));
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GET EXIT %d:0x%lx %6Lg",
	       thread, (long unsigned) offset, result);
  return result;
}

/**
 * Strict shared memory block get operation.
 * Copy the data at the shared address 'src' into the local memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Local address of the destination memory block.
 * @param [in] src Shared address of the source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__getsblk3 (void *dest, upc_shared_ptr_t src, size_t n)
{
  int thread = GUPCR_PTS_THREAD (src);
  size_t offset = GUPCR_PTS_OFFSET (src);
  gupcr_trace (FC_MEM, "GETBLK ENTER S");
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      GUPCR_MEM_BARRIER ();
      memcpy (dest, GUPCR_GMEM_OFF_TO_LOCAL (thread, offset), n);
      GUPCR_READ_MEM_BARRIER ();
    }
  else
    {
      gupcr_gmem_get (dest, thread, offset, n);
      /* All 'get' operations are synchronous.  */
      gupcr_gmem_sync_gets ();
    }
  gupcr_trace (FC_MEM, "GETBLK EXIT S %d:0x%lx 0x%lx %lu",
	       thread, (long unsigned) offset,
	       (long unsigned) dest, (long unsigned) n);
}

/**
 * Strict shared "char (8 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsqi2 (upc_shared_ptr_t p, u_intQI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S QI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(u_intQI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S QI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S QI");
}

/**
 * Strict shared "short (16 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putshi2 (upc_shared_ptr_t p, u_intHI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S HI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(u_intHI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S HI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S HI");
}

/**
 * Strict shared "int (32 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putssi2 (upc_shared_ptr_t p, u_intSI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S SI LOCAL "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(u_intSI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S SI REMOTE "
		   "0x%x %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S SI");
}

/**
 * Strict shared "long (64 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsdi2 (upc_shared_ptr_t p, u_intDI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S DI LOCAL "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(u_intDI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S DI REMOTE "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S DI");
}

#if GUPCR_TARGET64
/**
 * Strict shared "long long (128 bits)" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsti2 (upc_shared_ptr_t p, u_intTI_t v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S TI LOCAL "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(u_intTI_t *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S TI REMOTE "
		   "0x%llx %d:0x%lx",
		   (long long unsigned) v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S TI");
}
#endif /* GUPCR_TARGET64 */
/**
 * Strict shared "float" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putssf2 (upc_shared_ptr_t p, float v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S SF LOCAL "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(float *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S SF REMOTE "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S SF");
}

/**
 * Strict shared "double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsdf2 (upc_shared_ptr_t p, double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S DF LOCAL "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S DF REMOTE "
		   "%6g %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S DF");
}

/**
 * Strict shared "long double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putstf2 (upc_shared_ptr_t p, long double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S TF LOCAL "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S TF REMOTE "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S TF");
}

/**
 * Strict shared "long double" put operation.
 * Store the value given by 'v' into the shared memory destination at 'p'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] p Shared address of the destination address.
 * @param [in] v Source value.
 */
//inline
void
__putsxf2 (upc_shared_ptr_t p, long double v)
{
  int thread = GUPCR_PTS_THREAD (p);
  size_t offset = GUPCR_PTS_OFFSET (p);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      gupcr_trace (FC_MEM, "PUT ENTER S XF LOCAL "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      GUPCR_WRITE_MEM_BARRIER ();
      *(long double *) GUPCR_GMEM_OFF_TO_LOCAL (thread, offset) = v;
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_trace (FC_MEM, "PUT ENTER S XF REMOTE "
		   "%6Lg %d:0x%lx", v, thread, (long unsigned) offset);
      if (sizeof (v) <= (size_t) GUPCR_PORTALS_MAX_ORDERED_SIZE)
	{
	  /* Ordered puts can proceed in parallel.  */
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      else
	{
	  /* Wait for any outstanding 'put' operation.  */
	  gupcr_gmem_sync_puts ();
	  gupcr_gmem_put (thread, offset, &v, sizeof (v));
	}
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT EXIT S XF");
}

/**
 * Strict shared memory block put operation.
 * Copy the data at the local address 'src' into the shared memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Shared address of the destination memory block.
 * @param [in] src Local address of the source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__putsblk3 (upc_shared_ptr_t dest, void *src, size_t n)
{
  int thread = GUPCR_PTS_THREAD (dest);
  size_t offset = GUPCR_PTS_OFFSET (dest);
  gupcr_trace (FC_MEM, "PUTBLK ENTER S 0x%lx %d:0x%lx %lu",
	       (long unsigned) src, thread,
	       (long unsigned) offset, (long unsigned) n);
  gupcr_assert (thread < THREADS);
  gupcr_assert (offset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (thread))
    {
      GUPCR_WRITE_MEM_BARRIER ();
      memcpy (GUPCR_GMEM_OFF_TO_LOCAL (thread, offset), src, n);
      GUPCR_MEM_BARRIER ();
    }
  else
    {
      gupcr_gmem_put (thread, offset, src, n);
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "PUT_BLK EXIT S");
}

/**
 * Strict shared memory block copy operation.
 * Copy the data at the shared address 'src' into the shared memory
 * destination at the address 'dest'.
 *
 * The interface to this procedure is defined by the UPC compiler API.
 *
 * @param [in] dest Shared address of destination memory block.
 * @param [in] src Shared address of source memory block.
 * @param [in] n Number of bytes to transfer.
 */
//inline
void
__copysblk3 (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n)
{
  int dthread = GUPCR_PTS_THREAD (dest);
  size_t doffset = GUPCR_PTS_OFFSET (dest);
  int sthread = GUPCR_PTS_THREAD (src);
  size_t soffset = GUPCR_PTS_OFFSET (src);
  gupcr_trace (FC_MEM, "COPYBLK ENTER S %d:0x%lx %d:0x%lx %lu",
	       sthread, (long unsigned) soffset,
	       dthread, (long unsigned) doffset, (long unsigned) n);
  gupcr_assert (dthread < THREADS);
  gupcr_assert (doffset != 0);
  gupcr_assert (sthread < THREADS);
  gupcr_assert (soffset != 0);
  if (gupcr_pending_strict_put)
    gupcr_gmem_sync_puts ();
  if (GUPCR_GMEM_IS_LOCAL (dthread) && GUPCR_GMEM_IS_LOCAL (sthread))
    {
      GUPCR_WRITE_MEM_BARRIER ();
      memcpy (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
	      GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
      GUPCR_MEM_BARRIER ();
    }
  else if (GUPCR_GMEM_IS_LOCAL (dthread))
    {
      gupcr_gmem_get (GUPCR_GMEM_OFF_TO_LOCAL (dthread, doffset),
		      sthread, soffset, n);
      gupcr_gmem_sync_gets ();
    }
  else if (GUPCR_GMEM_IS_LOCAL (sthread))
    {
      gupcr_gmem_put (dthread, doffset,
		      GUPCR_GMEM_OFF_TO_LOCAL (sthread, soffset), n);
      gupcr_pending_strict_put = 1;
    }
  else
    {
      gupcr_gmem_copy (dthread, doffset, sthread, soffset, n);
      gupcr_pending_strict_put = 1;
    }
  gupcr_trace (FC_MEM, "COPY_BLK EXIT S");
}

/**
 * upc_fence implementation.
 */
//inline
void
__upc_fence (void)
{
  GUPCR_MEM_BARRIER ();
  gupcr_gmem_sync ();
}

//end lib_inline_access
/** @} */
