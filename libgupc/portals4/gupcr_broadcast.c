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
#include "gupcr_lib.h"
#include "gupcr_sup.h"
#include "gupcr_utils.h"
#include "gupcr_barrier.h"
#include "gupcr_broadcast.h"

/**
 * @file gupcr_broadcast.c
 * GUPC Portals4 broadcast implementation.
 *
 * The broadcast utility functions are internal functions used by
 * the UPC runtime when it is necessary to propagate (broadcast)
 * a value from thread 0 to all other threads.
 * For example, upc_all_alloc and upc_all_lock_alloc will
 * call the broadcast functions to propagate their result to all threads.
 *
 * The broadcast functions use an algorithm that is a variant of
 * the one used to implement a barrier.  The "up phase" signals
 * that each thread is ready to receive the broadcast value, while the
 * "down phase" is used to receive the actual value.
 */

/**
 * @addtogroup BROADCAST GUPCR Broadcast Functions
 * @{
 */

/**
 * Receive the broadcast value.
 *
 * A thread signals to its parent (by writing to its parent's UP PTE)
 * that it is ready to receive a broadcast value, and then waits on
 * its down LE counting event until the message arrives.
 * @param [in] value Pointer to received value
 * @param [in] nbytes Number of bytes to receive
 */
void
gupcr_broadcast_get (void *value, size_t nbytes)
{
  if (!MYTHREAD)
    gupcr_fatal_error ("Called from thread 0");
  if (nbytes > GUPCR_MAX_BROADCAST_SIZE)
    gupcr_fatal_error ("Size of the broadcast message (%lld) is greater then "
		       "the maximum allowed (%d)",
		       (long long int) nbytes, GUPCR_MAX_BROADCAST_SIZE);
  /* Wait to receive the broadcast message.  */
  gupcr_bcast_recv (value, nbytes);
}

/**
 * Send the broadcast value.  This function must be called
 * only by thread 0 (the root thread).  The broadcast value
 * is pushed down the tree by first sending the value
 * to the children of the root thread.
 *
 * @param [in] value Pointer to send value
 * @param [in] nbytes Number of bytes to send
 */
void
gupcr_broadcast_put (void *value, size_t nbytes)
{
  if (THREADS == 1)
    return;
  if (MYTHREAD)
    gupcr_fatal_error ("Thread %d: called from thread other then 0",
		       MYTHREAD);
  if (nbytes > GUPCR_MAX_BROADCAST_SIZE)
    gupcr_fatal_error ("Size of the broadcast message (%lld) is greater then "
		       "maximum allowed (%d)",
		       (long long int) nbytes, GUPCR_MAX_BROADCAST_SIZE);
  /* Send the broadcast message to the children of the root thread.  */
  gupcr_bcast_send (value, nbytes);
}

/**
 * Initialize broadcast resources.
 * @ingroup INIT
 */
void
gupcr_broadcast_init (void)
{
  gupcr_log (FC_BROADCAST, "broadcast init called");
}

/**
 * Release broadcast resources.
 * @ingroup INIT
 */
void
gupcr_broadcast_fini (void)
{
  gupcr_log (FC_BROADCAST, "broadcast fini called");
}

/** @} */
