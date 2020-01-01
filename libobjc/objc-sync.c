/* GNU Objective C Runtime @synchronized implementation
   Copyright (C) 2010-2020 Free Software Foundation, Inc.
   Contributed by Nicola Pero <nicola.pero@meta-innovation.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This file implements objc_sync_enter() and objc_sync_exit(), the
   two functions required to support @synchronized().

   objc_sync_enter(object) needs to get a recursive lock associated
   with 'object', and lock it.
   
   objc_sync_exit(object) needs to get the recursive lock associated
   with 'object', and unlock it.  */

/* To avoid the overhead of continuously allocating and deallocating
   locks, we implement a pool of locks.  When a lock is needed for an
   object, we get a lock from the pool and associate it with the
   object.
 
   The lock pool need to be protected by its own lock (the
   "protection" lock), which has to be locked then unlocked each time
   objc_sync_enter() and objc_sync_exit() are called.  To reduce the
   contention on the protection lock, instead of a single pool with a
   single (global) protection lock we use a number of smaller pools,
   each with its own pool protection lock.  To decide which lock pool
   to use for each object, we compute a hash from the object pointer.
 
   The implementation of each lock pool uses a linked list of all the
   locks in the pool (both unlocked, and locked); this works in the
   assumption that the number of locks concurrently required is very
   low.  In practice, it seems that you rarely see more than a few
   locks ever concurrently required.
 
   A standard case is a thread acquiring a lock recursively, over and
   over again: for example when most methods of a class are protected
   by @synchronized(self) but they also call each other.  We use
   thread-local storage to implement a cache and optimize this case.
   The cache stores locks that the thread successfully acquired,
   allowing objc_sync_enter() and objc_sync_exit() to locate a lock
   which is already held by the current thread without having to use
   any protection lock or synchronization mechanism.  It can so detect
   recursive locks/unlocks, and transform them into no-ops that
   require no actual locking or synchronization mechanisms at all.  */

/* You can disable the thread-local cache (most likely to benchmark
   the code with and without it) by compiling with
   -DSYNC_CACHE_DISABLE, or commenting out the following line.  */
/* #define SYNC_CACHE_DISABLE */

/* If thread-local storage is not available, automatically disable the
   cache.  */
#ifndef HAVE_TLS
# define SYNC_CACHE_DISABLE
#endif

#include "objc-private/common.h"
#include "objc/objc-sync.h"         /* For objc_sync_enter(), objc_sync_exit() */
#include "objc/runtime.h"           /* For objc_malloc() */
#include "objc/thr.h"               /* For objc_mutex_loc() and similar */
#include "objc-private/objc-sync.h" /* For __objc_sync_init() */

/* We have 32 pools of locks, each of them protected by its own
   protection lock.  It's tempting to increase this number to reduce
   contention; but in our tests it is high enough.  */
#define SYNC_NUMBER_OF_POOLS 32

/* Given an object, it determines which pool contains the associated
   lock.  */
#define SYNC_OBJECT_HASH(OBJECT) ((((size_t)OBJECT >> 8) ^ (size_t)OBJECT) & (SYNC_NUMBER_OF_POOLS - 1))

/* The locks protecting each pool.  */
static objc_mutex_t sync_pool_protection_locks[SYNC_NUMBER_OF_POOLS];

/* The data structure (linked list) holding the locks.  */
typedef struct lock_node
{
  /* Pointer to next entry on the list.  NULL indicates end of list.
     You need to hold the appropriate sync_pool_protection_locks[N] to
     read or write this variable.  */
  struct lock_node *next;

  /* The (recursive) lock.  Allocated when the node is created, and
     always not-NULL, and unchangeable, after that.  */
  objc_mutex_t lock;

  /* This is how many times the objc_mutex_lock() has been called on
     the lock (it is 0 when the lock is unused).  Used to track when
     the lock is no longer associated with an object and can be reused
     for another object.  It records "real" locks, potentially (but
     not necessarily) by multiple threads.  You need to hold the
     appropriate sync_pool_protection_locks[N] to read or write this
     variable.  */
  unsigned int usage_count;

  /* The object that the lock is associated with.  This variable can
     only be written when holding the sync_pool_protection_locks[N]
     and when node->usage_count == 0, ie, the lock is not being used.
     You can read this variable either when you hold the
     sync_pool_protection_locks[N] or when you hold node->lock,
     because in that case you know that node->usage_count can't get to
     zero until you release the lock.  It is valid to have usage_count
     == 0 and object != nil; in that case, the lock is not currently
     being used, but is still currently associated with the
     object.  */
  id object;

  /* This is a counter reserved for use by the thread currently
     holding the lock.  So, you need to hold node->lock to read or
     write this variable.  It is normally 0, and if the cache is not
     being used, it is kept at 0 (even if recursive locks are being
     done; in that case, no difference is made between recursive and
     non-recursive locks: they all increase usage_count, and call
     objc_mutex_lock()).  When the cache is being used, a thread may
     be able to find a lock that it already holds using the cache; in
     that case, to perform additional locks/unlocks it can
     increase/decrease the recursive_usage_count (which does not
     require any synchronization with other threads, since it's
     protected by the node->lock itself) instead of the usage_count
     (which requires locking the pool protection lock).  And it can
     skip the call to objc_mutex_lock/unlock too.  */
  unsigned int recursive_usage_count;
} *lock_node_ptr;


/* The pools of locks.  Each of them is a linked list of lock_nodes.
   In the list we keep both unlocked and locked nodes.  */
static lock_node_ptr sync_pool_array[SYNC_NUMBER_OF_POOLS];

#ifndef SYNC_CACHE_DISABLE
/* We store a cache of locks acquired by each thread in thread-local
   storage.  */
static __thread lock_node_ptr *lock_cache = NULL;

/* This is a conservative implementation that uses a static array of
   fixed size as cache.  Because the cache is an array that we scan
   linearly, the bigger it is, the slower it gets.  This does not
   matter much at small sizes (eg, the overhead of checking 8 cache
   slots instead of 4 is very small compared to the other overheads
   involved such as function calls and lock/unlock operations), but at
   large sizes it becomes important as obviously there is a size over
   which using the cache backfires: the lookup is so slow that the
   cache slows down the software instead of speeding it up.  In
   practice, it seems that most threads use a small number of
   concurrent locks, so we have a conservative implementation with a
   fixed-size cache of 8 locks which gives a very predictable
   behaviour.  If a thread locks lots of different locks, only the
   first 8 get the speed benefits of the cache, but the cache remains
   always small, fast and predictable.
 
   SYNC_CACHE_SIZE is the size of the lock cache for each thread.  */
#define SYNC_CACHE_SIZE 8
#endif /* SYNC_CACHE_DISABLE */

/* Called at startup by init.c.  */
void
__objc_sync_init (void)
{
  int i;

  for (i = 0; i < SYNC_NUMBER_OF_POOLS; i++)
    {
      lock_node_ptr new_node;
      
      /* Create a protection lock for each pool.  */
      sync_pool_protection_locks[i] = objc_mutex_allocate ();

      /* Preallocate a lock per pool.  */
      new_node = objc_malloc (sizeof (struct lock_node));
      new_node->lock = objc_mutex_allocate ();
      new_node->object = nil;
      new_node->usage_count = 0;
      new_node->recursive_usage_count = 0;
      new_node->next = NULL;

      sync_pool_array[i] = new_node;
    }
}  

int
objc_sync_enter (id object)
{
#ifndef SYNC_CACHE_DISABLE
  int free_cache_slot;
#endif
  int hash;
  lock_node_ptr node;
  lock_node_ptr unused_node;

  if (object == nil)
    return OBJC_SYNC_SUCCESS;

#ifndef SYNC_CACHE_DISABLE
  if (lock_cache == NULL)
    {
      /* Note that this calloc only happen only once per thread, the
	 very first time a thread does a objc_sync_enter().  */
      lock_cache = objc_calloc (SYNC_CACHE_SIZE, sizeof (lock_node_ptr));
    }

  /* Check the cache to see if we have a record of having already
     locked the lock corresponding to this object.  While doing so,
     keep track of the first free cache node in case we need it
     later.  */ 
  node = NULL;
  free_cache_slot = -1;

  {
    int i;
    for (i = 0; i < SYNC_CACHE_SIZE; i++)
      {
	lock_node_ptr locked_node = lock_cache[i];
	
	if (locked_node == NULL)
	  {
	    if (free_cache_slot == -1)
	      free_cache_slot = i;
	  }
	else if (locked_node->object == object)
	  {
	    node = locked_node;
	    break;
	  }
      }
  }

  if (node != NULL)
    {
      /* We found the lock.  Increase recursive_usage_count, which is
	 protected by node->lock, which we already hold.  */
      node->recursive_usage_count++;
      
      /* There is no need to actually lock anything, since we already
	 hold the lock.  Correspondingly, objc_sync_exit() will just
	 decrease recursive_usage_count and do nothing to unlock.  */
      return OBJC_SYNC_SUCCESS;
    }
#endif /* SYNC_CACHE_DISABLE */

  /* The following is the standard lookup for the lock in the standard
     pool lock.  It requires a pool protection lock.  */
  hash = SYNC_OBJECT_HASH(object);

  /* Search for an existing lock for 'object'.  While searching, make
     note of any unused lock if we find any.  */
  unused_node = NULL;

  objc_mutex_lock (sync_pool_protection_locks[hash]);

  node = sync_pool_array[hash];

  while (node != NULL)
    {
      if (node->object == object)
	{
	  /* We found the lock.  */
	  node->usage_count++;
	  objc_mutex_unlock (sync_pool_protection_locks[hash]);

#ifndef SYNC_CACHE_DISABLE
	  /* Put it in the cache.  */
	  if (free_cache_slot != -1)
	    lock_cache[free_cache_slot] = node;
#endif

	  /* Lock it.  */
	  objc_mutex_lock (node->lock);

	  return OBJC_SYNC_SUCCESS;
	}

      if (unused_node == NULL  &&  node->usage_count == 0)
	{
	  /* We found the first unused node.  Record it.  */
	  unused_node = node;
	}
      
      node = node->next;
    }

  /* An existing lock for 'object' could not be found.  */
  if (unused_node != NULL)
    {
      /* But we found a unused lock; use it.  */
      unused_node->object = object;
      unused_node->usage_count = 1;
      unused_node->recursive_usage_count = 0;
      objc_mutex_unlock (sync_pool_protection_locks[hash]);

#ifndef SYNC_CACHE_DISABLE
      if (free_cache_slot != -1)
	lock_cache[free_cache_slot] = unused_node;
#endif

      objc_mutex_lock (unused_node->lock);

      return OBJC_SYNC_SUCCESS;
    }
  else
    {
      /* There are no unused nodes; allocate a new node.  */
      lock_node_ptr new_node;

      /* Create the node.  */
      new_node = objc_malloc (sizeof (struct lock_node));
      new_node->lock = objc_mutex_allocate ();
      new_node->object = object;
      new_node->usage_count = 1;
      new_node->recursive_usage_count = 0;

      /* Attach it at the beginning of the pool.  */
      new_node->next = sync_pool_array[hash];
      sync_pool_array[hash] = new_node;
      objc_mutex_unlock (sync_pool_protection_locks[hash]);

#ifndef SYNC_CACHE_DISABLE
      if (free_cache_slot != -1)
	lock_cache[free_cache_slot] = new_node;
#endif

      objc_mutex_lock (new_node->lock);

      return OBJC_SYNC_SUCCESS;
    }
}

int
objc_sync_exit (id object)
{
  int hash;
  lock_node_ptr node;

  if (object == nil)
    return OBJC_SYNC_SUCCESS;
  
#ifndef SYNC_CACHE_DISABLE
  if (lock_cache != NULL)
    {
      int i;
    
      /* Find the lock in the cache.  */
      node = NULL;
      for (i = 0; i < SYNC_CACHE_SIZE; i++)
	{
	  lock_node_ptr locked_node = lock_cache[i];
	  
	  if (locked_node != NULL  &&  locked_node->object == object)
	    {
	      node = locked_node;
	      break;
	    }
	}
      /* Note that, if a node was found in the cache, the variable i
	 now holds the index where it was found, which will be used to
	 remove it from the cache.  */
      if (node != NULL)
	{
	  if (node->recursive_usage_count > 0)
	    {
	      node->recursive_usage_count--;
	      return OBJC_SYNC_SUCCESS;
	    }
	  else
	    {
	      /* We need to do a real unlock.  */
	      hash = SYNC_OBJECT_HASH(object);
	      
	      /* TODO: If we had atomic increase/decrease operations
		 with memory barriers, we could avoid the lock
		 here!  */
	      objc_mutex_lock (sync_pool_protection_locks[hash]);
	      node->usage_count--;
	      /* Normally, we do not reset object to nil here.  We'll
		 leave the lock associated with that object, at zero
		 usage count.  This makes it slightly more efficient to
		 provide a lock for that object if (as likely)
		 requested again.  If the object is deallocated, we
		 don't care.  It will never match a new lock that is
		 requested, and the node will be reused at some point.

		 But, if garbage collection is enabled, leaving a
		 pointer to the object in memory might prevent the
		 object from being released.  In that case, we remove
		 it (TODO: maybe we should avoid using the garbage
		 collector at all ?  Nothing is ever deallocated in
		 this file).  */
#if OBJC_WITH_GC
	      node->object = nil;
#endif
	      objc_mutex_unlock (sync_pool_protection_locks[hash]);
	    
	      /* PS: Between objc_mutex_unlock
		 (sync_pool_protection_locks[hash]) and
		 objc_mutex_unlock (node->lock), the pool is unlocked
		 so other threads may allocate this same lock to
		 another object (!).  This is not a problem, but it is
		 curious.  */
	      objc_mutex_unlock (node->lock);
	      
	      /* Remove the node from the cache.  */
	      lock_cache[i] = NULL;
	      
	      return OBJC_SYNC_SUCCESS;
	    }
	}
    }
#endif	  

  /* The cache either wasn't there, or didn't work (eg, we overflowed
     it at some point and stopped recording new locks in the cache).
     Proceed with a full search of the lock pool.  */
  hash = SYNC_OBJECT_HASH(object);

  objc_mutex_lock (sync_pool_protection_locks[hash]);

  /* Search for an existing lock for 'object'.  */
  node = sync_pool_array[hash];

  while (node != NULL)
    {
      if (node->object == object)
	{
	  /* We found the lock.  */
	  node->usage_count--;
	  objc_mutex_unlock (sync_pool_protection_locks[hash]);

	  objc_mutex_unlock (node->lock);

	  /* No need to remove the node from the cache, since it
	     wasn't found in the cache when we looked for it!  */
	  return OBJC_SYNC_SUCCESS;
	}
      
      node = node->next;
    }

  objc_mutex_unlock (sync_pool_protection_locks[hash]);

  /* A lock for 'object' to unlock could not be found (!!).  */
  return OBJC_SYNC_NOT_OWNING_THREAD_ERROR;
}
