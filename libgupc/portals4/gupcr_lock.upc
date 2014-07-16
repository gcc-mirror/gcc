/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
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
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_lock_sup.h"
#include "gupcr_lock.h"
#include "gupcr_barrier.h"

/**
 * @file gupcr_lock.upc
 * GUPC Portals4 UPC lock implementation.
 *
 * The GUPC lock functions use MCS locks as described in the
 * Mellor-Crummey and Scott paper: Algorithms for Scalable Synchronization
 * on Shared-Memory Multiprocessors, ACM Transaction on Computer Systems,
 * February 1991.
 *
 * A lock is a simple data structure that lives in the shared memory space.
 * A pointer is used to point to the last thread on the waiting list.
 * A lock is available if this pointer is NULL.  The following Portals
 * atomic operations are used:
 * - SWAP - determine if the lock is available
 * - CSWAP - determine if the lock can be released
 *
 * The GUPC Portals implementation of UPC locks has the following
 * characteristics:
 * - The lock object has affinity to the thread that creates the lock.
 *   If the collective function, upc_all_lock_alloc, is called, then
 *   the lock object will have affinity to thread 0.
 * - A thread's lock waiting queue link object has affinity to the
 *   waiting thread.
 * - Portals atomic functions (SWAP and CSWAP) are used to
 *   guarantee fair access and FIFO ordering for all waiting threads.
 * - A special Portals Table Entry (GUPCR_PTL_PTE_LOCK) is used to provide
 *   for signaling threads taken off the waiting list.
 *
 * @addtogroup LOCK GUPCR Lock Functions
 * @{
 */

typedef struct gupcr_lock_link_struct gupcr_lock_link_t;
typedef shared [] gupcr_lock_link_t *gupcr_lock_link_ref;

/** upc_lock_t is an opaque shared type.  The 'upc_lock_struct'
    structure describes the internal representation of the
    UPC lock type.  */
struct upc_lock_struct
{
  gupcr_lock_link_ref last;	/* Must be first.  */
  gupcr_lock_link_ref owner_link;
  upc_lock_t *free_link;
};

struct gupcr_lock_link_struct
{
  gupcr_lock_link_ref next;	/* Must be first.  */
  int signal;			/* Signal the lock ownership.  */
  int free;			/* Lock has been freed.  */
};

struct gupcr_lock_link_cache_struct
{
  gupcr_lock_link_t lock_links[GUPCR_MAX_LOCKS];
};
typedef struct gupcr_lock_link_cache_struct gupcr_lock_link_cache_t;
typedef shared gupcr_lock_link_cache_t *gupcr_lock_link_cache_ref;

/** Array of lock links managed as a per-thread free list.  */
static shared gupcr_lock_link_cache_t gupcr_lock_link_cache[THREADS];
/** Per thread lock link free list.  */
static gupcr_lock_link_ref gupcr_lock_links;

/** UPC lock free list.  */
static upc_lock_t *lock_free;

/* Heap allocator locks.  */
/** Heap region allocation lock.  */
shared upc_lock_t gupcr_heap_region_lock_data;
upc_lock_t *gupcr_heap_region_lock;
/** Global heap lock.  */
shared upc_lock_t gupcr_global_heap_lock_data;
upc_lock_t *gupcr_global_heap_lock;
/** Local heap locks.  */
shared upc_lock_t gupcr_local_heap_lock_data[THREADS];
upc_lock_t *gupcr_local_heap_lock;

/**
 * Initialize the heap allocator locks.
 *
 * All shared references must be local due to the fact
 * this is called before Portals has been initialized.
 */
void
gupcr_lock_heap_sup_init (void)
{
  if (!MYTHREAD)
    {
      upc_memset (&gupcr_heap_region_lock_data, '\0', sizeof (upc_lock_t));
      upc_memset (&gupcr_global_heap_lock_data, '\0', sizeof (upc_lock_t));
    }
  gupcr_heap_region_lock = &gupcr_heap_region_lock_data;
  gupcr_global_heap_lock = &gupcr_global_heap_lock_data;

  upc_memset (&gupcr_local_heap_lock_data[MYTHREAD],
	      '\0', sizeof (upc_lock_t));
  gupcr_local_heap_lock = &gupcr_local_heap_lock_data[MYTHREAD];
}

/**
 * Initialize the local lock free list.
 */
void
gupcr_lock_free_init (void)
{
  lock_free = NULL;
}

/**
 * Initialize the local lock link free list.
 *
 * gupcr_lock_link_init() is called before the UPC runtime
 * is fully initialized.  Care is taken to make no UPC shared
 * memory accesses.
 */
void
gupcr_lock_link_init (void)
{
  gupcr_lock_link_ref link;
  gupcr_lock_link_t *local_link;
  gupcr_lock_links = gupcr_lock_link_cache[MYTHREAD].lock_links;
  link = gupcr_lock_links;
  local_link = (gupcr_lock_link_t *) link;
  memset (local_link, '\0', sizeof (gupcr_lock_link_cache_t));
  for (int i = 0; i < (GUPCR_MAX_LOCKS - 1); ++i)
    (local_link++)->next = ++link;
}

/**
 * Release the lock link record.
 */
static inline void
gupcr_lock_link_free (gupcr_lock_link_ref link)
{
  link->next = gupcr_lock_links;
  gupcr_lock_links = link;
}

/**
 * Allocate a lock link record.
 *
 * A lock link is a data structure used to link together
 * all threads waiting on a particular lock.
 * Lock links are located in the shared space of the thread
 * acquiring the lock.
 *
 * Lock links are kept in a locally managed list
 * (used as a cache) rooted at 'gupcr_lock_links'.
 * This locally managed free list avoids the need
 * to call upc_alloc().
 */
static inline gupcr_lock_link_ref
gupcr_lock_link_alloc (void)
{
  gupcr_lock_link_ref link;
  link = gupcr_lock_links;
  if (!link)
    {
      /* Try to find a link block that has been freed by
         other threads and thus not returned to the free list.  */
      gupcr_lock_link_ref mlink = (gupcr_lock_link_ref)
	&gupcr_lock_link_cache[MYTHREAD].lock_links;
      for (int i = 0; i < (GUPCR_MAX_LOCKS - 1); ++i)
	{
	  if (mlink->free)
	    {
	      mlink->free = 0;
	      gupcr_lock_link_free (mlink);
	    }
	  mlink++;
	}
      link = gupcr_lock_links;
      if (!link)
	gupcr_fatal_error ("cannot allocate a UPC lock link. "
			   "The number of allocated per thread lock links "
			   "exceeds the configuration defined "
			   "maximum of %d entries.",
			   GUPCR_MAX_LOCKS);
    }
  gupcr_lock_links = link->next;
  return link;
}

/**
 * Allocate a lock and return a pointer to it.
 *
 * The 'upc_global_lock_alloc' function dynamically allocates a lock and
 * returns a pointer to it.  The lock is created in an unlocked state.
 * The 'upc_global_lock_alloc' function is not a collective function.
 * If called by multiple threads, each thread will receive a pointer
 * to a unique lock.
 *
 * @retval Pointer to a newly allocated lock
 */
upc_lock_t *
upc_global_lock_alloc (void)
{
  upc_lock_t *lock;
  gupcr_trace (FC_LOCK, "LOCK GLOBAL_ALLOC ENTER");
  /* Check if there is a lock cached on the free list.  */
  if (lock_free)
    {
      lock = lock_free;
      lock_free = lock->free_link;
    }
  else
    {
      /* Allocate space for the lock from shared memory with
         affinity to the calling thread.  */
      lock = upc_alloc (sizeof (upc_lock_t));
      if (lock == NULL)
	gupcr_fatal_error ("cannot allocate memory for the lock");
    }
  lock->last = NULL;
  lock->owner_link = NULL;
  gupcr_trace (FC_LOCK, "LOCK GLOBAL_ALLOC EXIT %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  return lock;
}

void
upc_all_lock_free (upc_lock_t *ptr)
{
  upc_all_free (ptr);
}

/**
 * Free all lock resources.
 *
 * The 'upc_lock_free' function frees all resources associated with the
 * dynamically allocated 'upc_lock_t' pointed to by 'lock'. If 'lock' is a
 * null pointer, no action occurs.  Otherwise, if the argument does not
 * match a pointer earlier returned by the 'upc_global_lock_alloc' or
 * 'upc_all_lock_alloc' function, or if the lock has been de-allocated by
 * a previous call to 'upc_lock_free' the behavior is undefined.
 *
 * @param [in] lock Pointer to a lock
 */
void
upc_lock_free (upc_lock_t *lock)
{
  gupcr_lock_link_ref link;

  gupcr_trace (FC_LOCK, "LOCK FREE ENTER %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  if (lock == NULL)
    return;

  link = lock->owner_link;
  /* Release the link block if this thread owns the lock.  */
  if (link)
    {
      if (MYTHREAD == (int) upc_threadof (link))
	{
	  gupcr_lock_link_free (link);
	}
      else
	link->free = 1;
    }
  if (MYTHREAD == (int) upc_threadof (lock))
    {
      /* Release it on the local free list.  */
      lock->free_link = lock_free;
      lock_free = lock;
    }
  else
    upc_free (lock);

  gupcr_trace (FC_LOCK, "LOCK FREE EXIT");
}

/**
 * Allocate a lock and return a pointer to it on all threads.
 *
 * The 'upc_all_lock_alloc' function dynamically allocates a lock
 * and returns a pointer to it.  The lock is created in an unlocked state.
 * 'upc_all_lock_alloc' is a collective function.
 * The return value on every thread points to the same lock object.
 *
 * @retval Pointer to a newly allocated lock
 */
upc_lock_t *
upc_all_lock_alloc (void)
{
  upc_lock_t *lock;
  gupcr_trace (FC_LOCK, "LOCK ALL_ALLOC ENTER");
  /* Allocate space for the lock from the shared memory of
     thread 0 and broadcast its address.  */
  if (MYTHREAD == 0)
    {
      if (lock_free)
	{
	  lock = lock_free;
	  lock_free = lock->free_link;
	}
      else
	{
	  lock = upc_alloc (sizeof (upc_lock_t));
	  if (lock == NULL)
	    gupcr_fatal_error ("cannot allocate memory for the lock");
	}
      lock->last = NULL;
      lock->owner_link = NULL;
      gupcr_bcast_send (&lock, sizeof (lock));
    }
  else
    gupcr_bcast_recv (&lock, sizeof (lock));
  gupcr_trace (FC_LOCK, "LOCK ALL_ALLOC EXIT %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  return lock;
}

/**
 * Set the state of the lock to locked.
 *
 * If the lock is already in a 'locked' state due to the calling thread
 * setting it to the 'locked' state, the result is undefined.
 * If the lock is already in a 'locked' state, then the calling thread
 * waits for some other thread to set the state to 'unlocked'.
 * Once the lock is in the state 'unlocked', a single calling thread
 * sets the state to 'locked' and the function returns.
 * A null strict access is implied after a call to 'upc_lock'.
 *
 * @param [in] lock Pointer to a lock
 */
void
upc_lock (upc_lock_t *lock)
{
  gupcr_lock_link_ref link, old_link;
  shared [] gupcr_lock_link_ref *lock_last_addr;
  size_t lock_last_thread, lock_last_offset;
  gupcr_trace (FC_LOCK, "LOCK LOCK ENTER %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  if (lock == NULL)
    gupcr_fatal_error ("NULL lock pointer");
  /* Allocate space for the lock waiting queue link.
     It will have affinity to the calling thread.  */
  link = gupcr_lock_link_alloc ();
  if (link == NULL)
    gupcr_fatal_error ("cannot allocate memory for the lock link");
  link->next = NULL;
  link->signal = 0;
  /* Atomically set the lock value to point to the
     calling thread's link queue object and
     return the previous value of the lock link.  */
  lock_last_addr = &lock->last;
  lock_last_thread = upc_threadof (lock_last_addr);
  lock_last_offset = upc_addrfield (lock_last_addr);
  gupcr_lock_swap (lock_last_thread, lock_last_offset,
		   &link, &old_link, sizeof (link));
  if (old_link != NULL)
    {
      shared [] gupcr_lock_link_ref *old_link_next_addr;
      size_t old_link_next_thread, old_link_next_offset;
      /* We have to wait.  Clear the ownership signal field
         and insert our pointer into the predecessor's link.  */
      link->signal = 0;
      upc_fence;
      old_link_next_addr = &old_link->next;
      old_link_next_thread = upc_threadof (old_link_next_addr);
      old_link_next_offset = upc_addrfield (old_link_next_addr);
      gupcr_lock_put (old_link_next_thread, old_link_next_offset,
		      &link, sizeof (link));
      /* At this point the thread has to wait until the lock is
         is released.  Process counting events one by one until
         the value of the signal word changes.  */
      do
	{
	  gupcr_lock_wait ();
	  upc_fence;
	}
      while (!link->signal);
    }
  lock->owner_link = link;
  gupcr_trace (FC_LOCK, "LOCK LOCK EXIT");
  upc_fence;
}

/**
 * Attempt to set the state of the lock to locked.
 *
 * The 'upc_lock_attempt' function attempts to set the state of the lock
 * pointed to by 'lock' to 'locked'.  If the lock is already in the 'locked'
 * state due to the calling thread setting it to the 'locked' state, the
 * result is undefined.  If the lock is already in the 'locked' state, the
 * function returns 0.  If the lock is in the state 'unlocked',
 * a single calling thread sets the state to 'locked' and the function
 * returns 1.  A null strict access is implied after a call to
 * 'upc_lock_attempt' that returns 1.
 *
 * @param [in] lock Pointer to a lock
 * @retval Lock attempt result
 *   - 1, lock was acquired successfully
 *   - 0, lock was not acquired
 */
int
upc_lock_attempt (upc_lock_t *lock)
{
  gupcr_lock_link_ref link;
  gupcr_lock_link_ref null_link = NULL;
  shared [] gupcr_lock_link_ref *lock_last_addr;
  size_t lock_last_thread, lock_last_offset;
  int compare_ok;
  gupcr_trace (FC_LOCK, "LOCK ATTEMPT ENTER %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  if (lock == NULL)
    gupcr_fatal_error ("NULL lock pointer");
  /* Allocate space for the lock waiting queue with affinity
     to the calling thread.  */
  link = gupcr_lock_link_alloc ();
  if (link == NULL)
    gupcr_fatal_error ("cannot allocate memory for the lock link");
  link->next = NULL;
  link->signal = 0;
  /* Atomically set the lock value to the link entry and
     return the previous value of the lock ONLY if the value
     of the lock is already NULL.  */
  lock_last_addr = &lock->last;
  lock_last_thread = upc_threadof (lock_last_addr);
  lock_last_offset = upc_addrfield (lock_last_addr);
  compare_ok = gupcr_lock_cswap (lock_last_thread, lock_last_offset,
				 &null_link, &link, sizeof (link));
  if (compare_ok)
    {
      lock->owner_link = link;
      upc_fence;
      gupcr_trace (FC_LOCK, "LOCK ATTEMPT EXIT 1");
    }
  else
    {
      gupcr_lock_link_free (link);
      gupcr_trace (FC_LOCK, "LOCK ATTEMPT EXIT 0");
    }
  return compare_ok;
}

/**
 * Set the state of the lock to unlocked.
 *
 * The 'upc_unlock' function sets the state of the lock pointed
 * to by 'lock' to 'unlocked'.  Unless the lock is in 'locked' state
 * and the calling thread is the locking thread, the result is undefined.
 * A null strict access is implied before a call to 'upc_unlock'.
 *
 * @param [in] lock Pointer to a lock
 */
void
upc_unlock (upc_lock_t *lock)
{
  gupcr_lock_link_ref link = lock->owner_link;
  gupcr_lock_link_ref null_link = NULL;
  shared [] gupcr_lock_link_ref *lock_last_addr = &lock->last;
  size_t lock_last_thread = upc_threadof (lock_last_addr);
  size_t lock_last_offset = upc_addrfield (lock_last_addr);
  int compare_ok;
  gupcr_trace (FC_LOCK, "LOCK UNLOCK ENTER %lu:0x%lx",
	       (long unsigned) upc_threadof (lock),
	       (long unsigned) upc_addrfield (lock));
  if (lock == NULL)
    gupcr_fatal_error ("NULL lock pointer");
  upc_fence;
  /* Try to release the lock: write NULL into lock->last
     if it contains a pointer to our own link block.  If it fails then
     some other thread is on the waiting list.  */
  lock->owner_link = NULL;
  compare_ok = gupcr_lock_cswap (lock_last_thread, lock_last_offset,
				 &link, &null_link, sizeof (link));
  if (!compare_ok)
    {
      shared void *link_next_signal_addr;
      size_t signal_addr_thread, signal_addr_offset;
      int signal = 1;
      /* Pass ownership to the next waiting thread.  Wait until
         the link->next pointer is being set.  Use Portals call to
	 avoid possibility of data tearing on pointer to shared.  */
      for (;;)
	{
          size_t addr_offset;
	  gupcr_lock_link_ref val;
          addr_offset = upc_addrfield (&link->next);
          gupcr_lock_get (MYTHREAD, addr_offset, &val, sizeof (val));
	  if (val) break;
	  gupcr_lock_wait ();
	}
      /* Signal the waiting thread that it now owns the lock.  */
      link_next_signal_addr = &link->next->signal;
      signal_addr_thread = upc_threadof (link_next_signal_addr);
      signal_addr_offset = upc_addrfield (link_next_signal_addr);
      gupcr_lock_put (signal_addr_thread, signal_addr_offset,
		      &signal, sizeof (signal));
    }
  gupcr_lock_link_free (link);
  gupcr_trace (FC_LOCK, "LOCK UNLOCK EXIT");
}

/** @} */
