/* Copyright (C) 2012
   Free Software Foundation, Inc.
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
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_slock.h"
#include "gupcr_lock_sup.h"
#include "gupcr_barrier.h"

/**
 * @file gupcr_slock.upc
 * GUPC Portals4 spin lock implementation.
 *
 * @addtogroup LOCK GUPCR Lock Functions
 * @{
 */

/** Clear a spin-lock.
 *
 * @param [in] lock Pointer to a spin lock
 */
void
gupcr_slock_reset (gupcr_slock_t *lock)
{
  lock->lock_word = 0;
}

/** Lock a spin-lock.
 *
 * @param [in] lock Pointer to a spin lock
 */
void
gupcr_slock_lock (gupcr_slock_t *lock)
{
  shared int *lock_word_addr;
  size_t lock_word_thread, lock_word_offset;
  unsigned int lock_set = 1;
  unsigned int lock_clear = 0;
  gupcr_assert (lock != NULL);
  lock_word_addr = (shared int *) &lock->lock_word;
  lock_word_thread = upc_threadof (lock_word_addr);
  lock_word_offset = upc_addrfield (lock_word_addr);
  unsigned int slot_count = ((MYTHREAD % GUPCR_SPIN_THREAD_SLOTS) + 1)
    * GUPCR_SPIN_SLOT_COUNT;
  unsigned int spin_count;
  upc_fence;
  for (int spin_mult = 1;
       !gupcr_lock_cswap (lock_word_thread, lock_word_offset,
			  &lock_clear, &lock_set, sizeof (lock_set));
       spin_mult *= 2)
    {
      if (spin_mult > GUPCR_SPIN_MAX_MULT)
	{
	  gupcr_yield_cpu ();
	  spin_mult = 1;
	  continue;
	}
      spin_count = slot_count * spin_mult;
      for (unsigned int c = 0; c < spin_count; ++c) /* spin */ ;
      upc_fence;
    }
}

/** Unlock a spin-lock.
 *
 * @param [in] lock Pointer to a spin lock
 */
void
gupcr_slock_unlock (gupcr_slock_t *lock)
{
  shared int *lock_word_addr;
  size_t lock_word_thread, lock_word_offset;
  unsigned int lock_set = 1;
  unsigned int lock_clear = 0;
  gupcr_assert (lock != NULL);
  lock_word_addr = (shared int *) &lock->lock_word;
  lock_word_thread = upc_threadof (lock_word_addr);
  lock_word_offset = upc_addrfield (lock_word_addr);

  if (!gupcr_lock_cswap (lock_word_thread, lock_word_offset,
			 &lock_set, &lock_clear, sizeof (lock_set)))
    gupcr_fatal_error ("[%d] Cannot release spin lock", MYTHREAD);
}

/** @} */
