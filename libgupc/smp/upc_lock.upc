/* Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
#include <string.h>
#include "config.h"
#include "upc_lock_sup.h"

/* UPC lock implementation.

   The UPC lock functions use MCS locks as described in the
   Mellor-Crummey and Scott paper: "Algorithms for Scalable Synchronization
   on Shared-Memory Multiprocessors", ACM Transaction on Computer Systems,
   February 1991.

   The following data structures are used in this implementation:

   * Lock link block
     A data structure in the shared address space used to link threads
     waiting on the lock.  Each thread inserts itself on the list with
     a link block that but has affinity to the thread itself. It has
     three fields: (1) next - link list pointer, (2) signal - notification
     of lock ownership transfer, (3) free flag - link block was freed by
     some other thread. A link block has affinity to the owner of the block.
   * Lock Link block reference
     A 64 bits container for pointer to shared that contains only a thread
     number and address field.  Link reference allow the lock routines to
     efficiently execute atomic operations on shared pointers.
   * Lock structure (upc_lock_t)
     A lock is a data structure living in the shared memory space.
     Contains two lock link references: (1) last - a lock link reference
     to the link block of the last thread on the lock waiting list,
     (2) owner - link reference to the lock's owner link block. The lock
     data structure has affinity of the thread that created the lock.

   The lock data structure goes though the following states:

   * Lock is free
     Last link block reference is NULL.
   * Lock is taken
     Last link block reference points to the last thread's link block on
     the waiting queue.  Owner link reference points to the owner thread's
     link block.  Only the owner of the lock is allowed to manipulate the
     owner's link reference in the lock data structure.

   The following operations are performed on the lock data structure:

   * Lock acquire
     Thread allocates a link block and write its reference to the 'last'
     filed of the lock data structure by performing an atomic SWAP
     operation.  If returned value is NULL, thread is the owner of the
     lock.  Otherwise a link reference to the last thread on the waiting
     queue is returned and thread needs to link itself on the waiting
     queue.
   * Lock release
     Attempt to write a NULL link block reference into the lock's 'last'
     field with atomic CSWAP operation.  If successful, lock is released.
     Otherwise, the ownership of the lock must be passed to the first
     thread on the wait queue.
   * Lock allocation/free
     Lock is allocated from the shared memory space or from the local
     free list.  They are freed by placing the lock data structure on
     the local lock free list if the lock has affinity of the thread that
     releases it.  Otherwise lock's memory is released.
*/

struct upc_lock_link_cache_struct
{
  upc_lock_link_t lock_links[GUPCR_MAX_LOCKS];
};
typedef struct upc_lock_link_cache_struct upc_lock_link_cache_t;

/* Array of lock links managed as a per-thread free list.  */
static shared upc_lock_link_cache_t upc_lock_link_cache[THREADS];

/* Per thread lock link free list.  */
static upc_lock_link_t *upc_lock_links;
/* Null link block reference.  Used for CSWAP operations.  */
upc_link_ref null_link = {.atomic = 0 };

/* UPC lock free list.  */
static upc_lock_t *lock_free;

/* Memory allocation support.  */
upc_lock_t *shared __upc_all_lock;
shared upc_lock_t __upc_alloc_lock;

__attribute__ ((__always_inline__))
static inline
void
upc_new_lock_init (upc_lock_t *lock)
{
  lock->last.atomic = 0;
  lock->owner_link.atomic = 0;
}

/* Lock link block utilities.  */

/* Lock link block is a data structure that links
   lock waiting threads.  It is located in the
   shared space of the thread waiting for the lock.
   They are locally managed with a free list rooted
   at 'upc_lock_links'.

   NOTE:
   The current design for memory allocation uses
   UPC locks in alloc/free routines.  Thus, link blocks
   cannot be allocate with the UPC memory allocation
   routines.  */

/* Initialize lock link block free list.
   NOTE: Link with local addresses for faster access.  */
static void
upc_lock_link_init (void)
{
  shared [] upc_lock_link_t *slink = upc_lock_link_cache[MYTHREAD].lock_links;
  upc_lock_link_t *link = (upc_lock_link_t *) slink;
  upc_lock_links = link;
  memset (link, '\0', sizeof (upc_lock_link_cache_t));
  for (int i = 0; i < (GUPCR_MAX_LOCKS - 1); i++)
    {
      link[i].link_ref = upc_to_link_ref (slink++);
      link[i].link = &link[i + 1];
    }
  link[GUPCR_MAX_LOCKS - 1].link_ref = upc_to_link_ref (slink++);
}

/* Release lock link block.  */
__attribute__ ((__always_inline__))
static inline
void
upc_lock_link_free (upc_lock_link_t * link)
{
  SET_NULL_LOCK_REF (link->next);
  link->signal = 0;
  link->link = upc_lock_links;
  upc_lock_links = link;
}

/* Allocate lock link block.  */
__attribute__ ((__always_inline__))
static inline
upc_lock_link_t *
upc_lock_link_alloc (void)
{
  upc_lock_link_t *link = upc_lock_links;
  if (!link)
    {
      /* Try to find a link block that has been freed by
         some other thread and thus not returned to the free list.  */
      upc_lock_link_t *llink = (upc_lock_link_t *)
	upc_lock_link_cache[MYTHREAD].lock_links;
      for (int i = 0; i < (GUPCR_MAX_LOCKS - 1); ++i)
	{
	  if (llink->free)
	    {
	      llink->free = 0;
	      upc_lock_link_free (llink);
	    }
	  llink++;
	}
      link = upc_lock_links;
      if (!link)
	__upc_fatal ("Cannot allocate a UPC lock link. "
		"The number of allocated per thread lock links "
		"exceeds the configuration defined maximum of entries.");
    }
  upc_lock_links = link->link;
  return link;
}

/* Allocate a lock and return a pointer to it.
   This is not a collective function.  */
upc_lock_t *
upc_global_lock_alloc (void)
{
  upc_lock_t *lock;
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
	__upc_fatal ("Cannot allocate memory for the lock");
    }
  upc_new_lock_init (lock);
  return lock;
}

/* Free all lock resources.
   If lock has affinity to the calling thread it is released on the
   local free list.  If 'lock' is a null pointer, no action occurs.
   Otherwise, if the argument does not match a pointer earlier
   returned by the alloc function, or if the lock has been de-allocated
   by a previous call to 'upc_lock_free' the behavior is undefined.  */

void
upc_lock_free (upc_lock_t *lock)
{
  upc_link_ref owner;
  if (lock == NULL)
    return;
  /* Release the link block if this thread owns the lock.  */
  owner = lock->owner_link;
  if (!NULL_LOCK_REF (owner))
    {
      shared upc_lock_link_t *link = upc_from_link_ref (owner);
      if (MYTHREAD == (int) upc_threadof (link))
	{
	  upc_lock_link_free ((upc_lock_link_t *) link);
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
}

/* Collective free all lock resources.  */
void
upc_all_lock_free (upc_lock_t *lock)
{
  upc_link_ref owner;
  if (lock == NULL)
    return;
  /* Release the link block if this thread owns the lock.  */
  owner = lock->owner_link;
  if (!NULL_LOCK_REF (owner))
    {
      shared upc_lock_link_t *link = upc_from_link_ref (owner);
      if (MYTHREAD == (int) upc_threadof (link))
	{
	  upc_lock_link_free ((upc_lock_link_t *) link);
	}
    }
  if (MYTHREAD == (int) upc_threadof (lock))
    {
      /* Release it on the local free list.  */
      lock->free_link = lock_free;
      lock_free = lock;
    }
  upc_barrier;
}

/* Allocate a lock and return a pointer to it.
   'upc_all_lock_alloc' is a collective function.  */
upc_lock_t *
upc_all_lock_alloc (void)
{
  upc_lock_t *lock;
  upc_barrier (-1);
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
	    __upc_fatal ("Cannot allocate memory for the lock");
	}
      upc_new_lock_init (lock);
      __upc_all_lock = lock;
    }
  upc_barrier (-1);
  return __upc_all_lock;
}

/* UPC lock acquire.  */
void
upc_lock (upc_lock_t *lock)
{
  upc_lock_link_t *link;
  upc_link_ref old_link_ref;
  link = upc_lock_link_alloc ();

  /* Insert this thread on the waiting list.  */
  upc_link_ref_swap (&lock->last, &old_link_ref, link->link_ref);
  if (!NULL_LOCK_REF (old_link_ref))
    {
      /* We have to wait.  "old_link_ref" contains a reference
         to the last thread on the wait queue.  */
      shared upc_lock_link_t *rmt_link = upc_from_link_ref (old_link_ref);
      upc_link_ref_put ((shared upc_link_ref *) &rmt_link->next,
			link->link_ref);
      /* Wait for lock ownership notification.  */
      __upc_spin_until (link->signal);
    }
  lock->owner_link = link->link_ref;
  upc_fence;
}

/* UPC lock acquire attempt.
   Return 1 if lock is acquired, 0 otherwise.  */
int
upc_lock_attempt (upc_lock_t *lock)
{
  upc_lock_link_t *link;
  int compare_ok;
  /* No need go further if lock is unavailable.  */
  if (!NULL_LOCK_REF (upc_link_ref_last (&lock->last)))
    return 0;
  /* Try to allocate the lock.  */
  link = upc_lock_link_alloc ();
  compare_ok = upc_link_ref_cswap (&lock->last, null_link, link->link_ref);
  if (compare_ok)
    {
      lock->owner_link = link->link_ref;
      upc_fence;
    }
  else
    {
      upc_lock_link_free (link);
    }
  return compare_ok;
}

/* UPC lock release.  */
void
upc_unlock (upc_lock_t *lock)
{
  upc_lock_link_t *link;
  upc_link_ref link_ref = lock->owner_link;
  int compare_ok;

  if (!lock)
    __upc_fatal ("Trying to release a NULL lock");
  if (NULL_LOCK_REF (link_ref))
    __upc_fatal ("Trying to release a lock that is not locked");
  upc_fence;
  link = (upc_lock_link_t *) upc_from_link_ref (link_ref);

  /* Try to release the lock by trying to write a NULL into lock
     block (last).  Use CSWAP with link_ref as expected.  */
  compare_ok = upc_link_ref_cswap (&lock->last, link_ref, null_link);
  if (!compare_ok)
    {
      /* Another thread is already waiting for the lock,
         pass the ownership.  */
      /* Make sure that waiting thread completed insertion on the
         waiting list.  */
      __upc_spin_until (!NULL_LOCK_REF (upc_link_ref_get (&link->next)));
      /* Notify the waiting thread that it now owns the lock.  */
      {
	shared upc_lock_link_t *rmt_link;
	rmt_link = upc_from_link_ref (link->next);
	rmt_link->signal = 1;
      }
    }
  upc_lock_link_free (link);
}

/* Heap manager lock support.  */

void
__upc_acquire_alloc_lock ()
{
  upc_lock (&__upc_alloc_lock);
}

void
__upc_release_alloc_lock ()
{
  upc_unlock (&__upc_alloc_lock);
}

/* Initialize UPC lock resources.  */
void
__upc_lock_init (void)
{
  upc_lock_link_init ();
  lock_free = NULL;

  /* Heap manager lock must be manually initialized.  */
  if (!MYTHREAD)
    upc_new_lock_init (&__upc_alloc_lock);
}

/** @} */
